(*
                                CS 51
                             Spring 2018
                        Problem Set 5: Search

                             Game Solving

This file contains the signature for a GAMESOLVER module as well as a
higher-order functor, MakeGameSolver. A GAMESOLVER module solves the
game by searching for a path from the initial state to the goal state.

The MakeGameSolver functor takes in a collection functor and a
GAMEDESCRIPTION and returns a GAMESOLVER. The collection specified by
the functor is used to store the states that have been reached so
far. Thus, the ordering in which states are delivered by the
collection (with the take function) determines the order in which the
states are searched. A stack regime gives depth-first search, a queue
regime breadth-first search.

At the bottom of the file are definitions for a depth-first search and
breadth-first game solvers, partially applied versions of the
MakeGameSolver functor that use certain collections to engender
different search methods.
 *)

open Set
open Collections
open Gamedescription

(* GAMESOLVER -- a module type that provides for solving games and
   graphically drawing the results *)

module type GAMESOLVER =
  sig
    exception CantReachGoal
    type state
    type move
    val solve : unit -> move list * state list
    val draw : state list -> move list -> unit
    val print_state: state -> unit
  end

(* MakeGameSolver -- a higher-order functor that generates game solvers, with type

  (functor(sig type t end -> COLLECTION)) -> GAMEDESCRIPTION -> GAMESOLVER

   A functor that given a functor from a GAMEDESCRIPTION to a
   game description-specific Collection, as well as a GAMEDESCRIPTION,
   returns a full GAMESOLVER module.
 *)

module MakeGameSolver (DSFunc : functor(Element : sig type t end) ->
                                       (COLLECTION with type elt = Element.t))
                      (G : GAMEDESCRIPTION)
       : (GAMESOLVER with type state = G.state
                      and type move = G.move) =
  struct
    exception CantReachGoal

    type state = G.state

    type move = G.move

    module States = DSFunc (struct type t = state * (move list) end)

    let solve () : move list * state list =

      let convert (sm : (state * move) list) : States.collection =
        (*takes in a state * move list and outputs a state * (move list) list *)
        let rec makemovelist (sml : (state * move) list) : move list =
        match sml with
        | [] -> []
        | hd::tl -> (snd hd)::(makemovelist tl) in

        let rec makestatemovelist (sm : (state * move) list) (m : move list) (i : int) : States.collection =
          let rec getmoves (x : move list) (y : int) : move list =
            match x, y with
            | _, 0 -> []
            | [], _ -> []
            | hd::tl, _ -> hd::(getmoves tl (y - 1)) in
          match sm with
          | [] -> States.empty
          | hd::tl -> States.add (fst hd, getmoves m i) (makestatemovelist tl m (i + 1)) in

        makestatemovelist sm (makemovelist sm) 0 in

      let rec combine_collections (a : States.collection) (b : States.collection) : States.collection =
        if States.is_empty b then a else combine_collections (States.add (fst (States.take b)) a) (snd (States.take b)) in

      (*CURRENT PROBLEM: TRYING TO FIND HOW THIS ITERATE FUNCTION IS RECURSING INFINITELY
      THIS FUNCTION ALSO DOES NOT RETURN A PROPER MOVE LIST*)
      let rec iterate (t : States.collection) (s : state list) : move list * state list =
        let x = (fst (fst (States.take t))) in
        if G.is_goal x then (snd (fst (States.take t)), s)
        else if States.is_empty t then raise CantReachGoal
        else iterate (combine_collections (snd (States.take t)) (convert (G.neighbors x))) (s @ [x])
      in

      (*BELOW HERE IS ME TRYING TO FIGURE OUT ANOTHER IMPLEMENTATION THAT ONLY TAKES
      STATES.COLLECTION AS INPUT. INCREMEMNT MOVES IF USED RIGHT SHOULD FIX THE MOVE LIST ISSUE*)
      let incrementmoves (sc : States.collection) (m : move) : States.collection =
        let helper (sm : state * (move list)) : state * (move list) =
          match sm with
          | (x, y) -> (x, y::[m]) in
        map helper sc in

      let rec buildstates (t : States.collection) : state list =
        let x = (fst (fst (States.take t))) in
        if G.is_goal x then x
        else if States.is_empty t then raise CantReachGoal
        else [x] @ buildstates (incrementmoves (combine_collections (snd (States.take t)) (convert (G.neighbors x))) ) in

      iterate (convert (G.neighbors G.initial_state)) [G.initial_state]

    let draw (sl : state list) (ml : move list) =
      G.draw sl ml

    let print_state (s : state) =
      G.print_state s

  end ;;

(* DFSSolver and BFSSolver: Higher-order Functors that take in a
   GAMEDESCRIPTION, and will return Games that are solved with DFS and
   BFS, respectively. The fast bfs solver uses a better implementation
   of queues for speed. *)
module DFSSolver = MakeGameSolver(MakeStackList) ;;
module BFSSolver = MakeGameSolver(MakeQueueList) ;;
module FastBFSSolver = MakeGameSolver(MakeQueueStack) ;;


(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) the problem set took you to complete.  We care about your
responses and will use them to help guide us in creating future
assignments.
......................................................................*)

let minutes_spent_gamesolve () : int = 400 ;;
let minutes_spent_writeup () : int = failwith "not provided" ;;
