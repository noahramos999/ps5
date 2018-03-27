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

    let get_moves (x : (state * move) list) : move list =
      let helper (a : state * move) : move =
        match a with
        | (a1, a2) -> a2 in
      map helper x

    let rec combine_collections (a : 'a States.collection) (b : 'a States.collection) : 'a States.collection =
      if b = States.empty then a else States.add States.add
    let take_elt (x : States.elt * States.collection) : States.elt =
      match x with
      | (a, b) -> a
    let take_collect (y : States.elt * States.collection) : States.collection =
      match y with
      | (a, b) -> b

    let solve () : move States.collection * state States.collection =
      let optionlistmaker (s : state) : move States.collection =
        get_moves (G.neighbors s) in
      let rec iterate (t : state) : move States.collection =
        match States.take optionlistmaker t with
        | (x, y) -> y

    let draw (sl : state list) (ml : move list) : () =
      G.draw sl ml

    let print_state (s : state) : () =
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

let minutes_spent_gamesolve () : int = failwith "not provided" ;;
let minutes_spent_writeup () : int = failwith "not provided" ;;
