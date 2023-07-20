open Graph
open Parany
module T = Domainslib.Task

(** [Queue] is a module that implements a queue data structure using two stacks.
    The first stack is used for enqueueing elements, while the second stack 
    is used for dequeueing elements. *)
module Queue : sig
  type 'a t

  val create_empty_queue : unit -> 'a t
  (** [create_empty_queue ()] creates an empty queue. *)

  val is_empty : 'a t -> bool
  (** [is_empty q] returns [true] if the queue [q] is empty, [false] otherwise. *)

  val enqueue : 'a -> 'a t -> 'a t
  (** [enqueue x q] adds element [x] to the queue [q]. *)

  val dequeue : 'a t -> ('a * 'a t) option
  (** [dequeue q] removes and returns the first element of the queue [q].
    If the queue is empty, returns [None]. *)
end = struct
  type 'a t = { enqueue_stack : 'a list; dequeue_stack : 'a list }

  let create_empty_queue () = { enqueue_stack = []; dequeue_stack = [] }
  let is_empty q = q.enqueue_stack = [] && q.dequeue_stack = []
  let enqueue x q = { q with enqueue_stack = x :: q.enqueue_stack }

  let dequeue q =
    match q.dequeue_stack with
    | hd :: tl -> Some (hd, { q with dequeue_stack = tl })
    | [] -> (
        match List.rev q.enqueue_stack with
        | [] -> None
        | hd :: tl -> Some (hd, { enqueue_stack = []; dequeue_stack = tl }))
end

(** This module type will implement the breadth-first search algorithm on a graph. 
    It provides two functions, [parallel] and [sequential], that take a graph 
    and a starting node and return a list of sets of nodes, where each set 
    contains all nodes at a certain distance from the starting node. 
    The [parallel] function uses parallelism to speed up the computation, 
    while the [sequential] function runs on a single thread. 
    *)
module type Bfs = sig
  val parallel : Graph.t -> Node.t -> num_domains:int -> NodeSet.t list
  (** [parallel graph start_node] is a function that implements the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
  It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
  This function uses parallelism to speed up the computation. *)

  val sequential : Graph.t -> Node.t -> NodeSet.t list
  (** [sequential graph start_node] is a function that implements the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
  It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
  This function runs on a single thread. *)
end

(** [Bfs] is a module that implements the breadth-first search algorithm. *)

module BfsAlgorithms : Bfs = struct
  (** [visit_node node visited graph] returns a list of nodes that are neighbours of [node] in [graph] and have not been visited yet. *)
  let visit_node (node : Node.t) (visited : NodeSet.t) (graph : Graph.t) :
      Node.t list =
    let neighbours : Node.t list = Graph.neighbours node graph in
    List.filter (fun node -> not (NodeSet.mem node visited)) neighbours

  let rec loop (visited : NodeSet.t) (stages : NodeSet.t list)
      (next_stage_implementation :
        NodeSet.t -> NodeSet.t -> Graph.t -> NodeSet.t) (graph : Graph.t) :
      NodeSet.t list =
    match stages with
    | last_stage :: _ ->
        let next : NodeSet.t =
          next_stage_implementation visited last_stage graph
        in
        if NodeSet.is_empty next then List.rev stages
        else
          loop
            (NodeSet.union visited next)
            (next :: stages) next_stage_implementation graph
    | [] -> failwith "Should not happen"

  (** [parallel graph start_node] is a function that implements the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
    It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
    This function uses parallelism to speed up the computation. *)
  let parallel (graph : Graph.t) (start_node : Node.t) ~(num_domains : int) :
      NodeSet.t list =
    let next_stage_par (visited : NodeSet.t) (previous_stage : NodeSet.t)
        (graph : Graph.t) : NodeSet.t =
      let new_nodes : Node.t list =
        previous_stage |> NodeSet.elements
        |> Parmap.parmap num_domains (fun node -> visit_node node visited graph)
        |> List.flatten
      in
      List.fold_left
        (fun set node -> NodeSet.add node set)
        NodeSet.empty new_nodes
    in
    loop
      (NodeSet.singleton start_node)
      [ NodeSet.singleton start_node ]
      next_stage_par graph

  (** [sequential graph start_node] is a function that implements the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
    It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
    This function runs on a single thread. *)
  let sequential (graph : Graph.t) (start_node : Node.t) : NodeSet.t list =
    let next_stage_seq (visited : NodeSet.t) (previous_stage : NodeSet.t)
        (graph : Graph.t) : NodeSet.t =
      let new_nodes : Node.t list =
        previous_stage |> NodeSet.elements
        |> List.map (fun node -> visit_node node visited graph)
        |> List.flatten
      in
      List.fold_left
        (fun set node -> NodeSet.add node set)
        NodeSet.empty new_nodes
    in
    loop
      (NodeSet.singleton start_node)
      [ NodeSet.singleton start_node ]
      next_stage_seq graph
end

module MakeBfsPerformanceAnalysis (Bfs : Bfs) : sig
  val bfs_par_calculation_time : Graph.t -> Node.t -> int -> float
  val bfs_seq_calculation_time : Graph.t -> Node.t -> float
  val bfs_par_calculation_time_num_domains_to_csv : Graph.t -> Node.t -> max_domains:int -> unit
  val bfs_calculation_time_combinations_to_csv : (int * int) list -> num_domains:int -> unit
end = struct
  (** [bfs_par_calculation_time graph start_node num_domains] runs the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
    It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
    This function uses parallelism to speed up the computation. *)
  let bfs_par_calculation_time (graph : Graph.t) (start_node : Node.t)
      (num_domains : int) : float =
    let start_time = Unix.gettimeofday () in
    let _ = Bfs.parallel graph start_node ~num_domains in
    (* Printf.printf "Parallel BFS took %f seconds\n" *)
    Unix.gettimeofday () -. start_time
    (* result *)

  (** [bfs_seq_calculation_time graph start_node] runs the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
    It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
    This function runs on a single thread. *)
  let bfs_seq_calculation_time (graph : Graph.t) (start_node : Node.t) : float =
    let start_time = Unix.gettimeofday () in
    let _ = Bfs.sequential graph start_node in
    (* Printf.printf "Sequential BFS took %f seconds\n" *)
    Unix.gettimeofday () -. start_time
  
  let bfs_par_calculation_time_num_domains_to_csv
      (graph : Graph.t) (start_node : Node.t) ~(max_domains:int) : unit =
    let out_channel = open_out "computation_time_analysis/bfs_par_domains.csv" in
    let rec bfs_par_calculation_time_num_domains_to_csv_aux num_domains =
      if num_domains > max_domains then ()
      else
        let calculation_time =
          bfs_par_calculation_time graph start_node num_domains
        in
        Printf.fprintf out_channel "%d,%.3f\n" num_domains calculation_time;
        bfs_par_calculation_time_num_domains_to_csv_aux (num_domains + 1)
    in
    output_string out_channel "num_domains,time\n";
    bfs_par_calculation_time_num_domains_to_csv_aux 1;
    close_out out_channel
  
    let bfs_calculation_time_combinations_to_csv (combinations : (int * int) list) ~(num_domains:int) : unit =
      let out_channel = open_out "computation_time_analysis/bfs_par_combinations.csv" in
      let calculate_and_write (num_nodes, num_edges) =
        let graph = Graph.create_new_graph ~num_nodes ~num_edges ~directed:false in
        let start_node = Option.get (Graph.find_node_by_id 1 graph) in
        let parallel_calculation_time =
          bfs_par_calculation_time graph start_node num_domains
        in
        let sequential_calculation_time =
          bfs_seq_calculation_time graph start_node
        in
        Printf.fprintf out_channel "%d,%d,%.3f,%.3f\n" num_nodes num_edges parallel_calculation_time sequential_calculation_time
      in
      output_string out_channel "num_nodes,num_edges,parallel_time,sequential_time\n";
      List.iter calculate_and_write combinations;
      close_out out_channel    
  
end

(** [BfsPerformanceAnalysis] is a module that provides performance analysis functions for the breadth-first search algorithm. *)
module BfsPerformanceAnalysis = MakeBfsPerformanceAnalysis (BfsAlgorithms)
