open Graph
module T = Domainslib.Task

(** This module type will implement the breadth-first search algorithm on a graph. 
    It provides two functions, [parallel] and [sequential], that take a graph 
    and a starting node and return a list of sets of nodes, where each set 
    contains all nodes at a certain distance from the starting node. 
    The [parallel] function uses parallelism to speed up the computation, 
    while the [sequential] function runs on a single thread. 
    *)
module type Bfs = sig
  val parallel :
    UnweightedGraph.t -> Node.t -> task_pool:T.pool -> NodeSet.t list
  (** [parallel graph start_node] is a function that implements the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
  It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
  This function uses parallelism to speed up the computation. *)

  val sequential : UnweightedGraph.t -> Node.t -> NodeSet.t list
  (** [sequential graph start_node] is a function that implements the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
  It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
  This function runs on a single thread. *)
end

(** [Bfs] is a module that implements the breadth-first search algorithm. *)

module BfsAlgorithms : Bfs = struct

  let rec loop (visited : NodeSet.t) (stages : NodeSet.t list)
      (next_stage_implementation :
        NodeSet.t -> NodeSet.t -> UnweightedGraph.t -> NodeSet.t)
      (graph : UnweightedGraph.t) : NodeSet.t list =
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

  let parallel_map (f : 'a -> 'b) (task_pool : T.pool) (arr : 'a array) : 'b array =
    let len = Array.length arr in
    let res = Array.make len (f arr.(0)) in
    T.parallel_for task_pool ~start:0 ~finish:(len - 1) ~body:(fun i ->
        res.(i) <- f arr.(i));
    res
  
  (** [parallel graph start_node] is a function that implements the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
    It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
    This function uses parallelism to speed up the computation. *)
  let parallel (graph : UnweightedGraph.t) (start_node : Node.t) ~(task_pool: T.pool) : NodeSet.t list =
    let next_stage_par (visited : NodeSet.t) (previous_stage : NodeSet.t)
        (graph : UnweightedGraph.t) : NodeSet.t =
      
      let new_nodes : Node.t list = previous_stage 
        |> NodeSet.elements
        |> Array.of_list
        |> parallel_map (fun node -> UnweightedGraph.neighbours node graph) task_pool
        |> Array.to_list
        |> List.fold_left NodeSet.union NodeSet.empty
        |> NodeSet.diff visited
        |> NodeSet.elements
      in
      List.fold_left (fun set node -> NodeSet.add node set) NodeSet.empty new_nodes
    in
    loop
      (NodeSet.singleton start_node)
      [ NodeSet.singleton start_node ]
      next_stage_par graph

  (** [sequential graph start_node] is a function that implements the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
    It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
    This function runs on a single thread. *)
  let sequential (graph : UnweightedGraph.t) (start_node : Node.t) :
      NodeSet.t list =
    let next_stage_seq (visited : NodeSet.t) (previous_stage : NodeSet.t)
        (graph : UnweightedGraph.t) : NodeSet.t =
      let new_nodes : Node.t list =
        previous_stage |> NodeSet.elements
        |> List.map (fun node -> UnweightedGraph.neighbours node graph)
        |> List.fold_left NodeSet.union NodeSet.empty
        |> NodeSet.elements
        |> List.filter (fun node -> not (NodeSet.mem node visited))
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
  val bfs_par_calculation_time : UnweightedGraph.t -> Node.t -> task_pool:T.pool -> float
  val bfs_seq_calculation_time : UnweightedGraph.t -> Node.t -> float

  (* val bfs_par_calculation_time_num_domains_to_csv :
    UnweightedGraph.t -> Node.t -> max_domains:int -> unit *)

  val bfs_calculation_time_combinations_to_csv :
    (int * int) list -> task_pool:T.pool -> unit
end = struct
  (** [bfs_par_calculation_time graph start_node num_domains] runs the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
    It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
    This function uses parallelism to speed up the computation. *)
  let bfs_par_calculation_time (graph : UnweightedGraph.t) (start_node : Node.t)
      ~(task_pool: T.pool) : float =
    let start_time = Unix.gettimeofday () in
    let _ = Bfs.parallel graph start_node ~task_pool in
    (* Printf.printf "Parallel BFS took %f seconds\n" *)
    Unix.gettimeofday () -. start_time
  (* result *)

  (** [bfs_seq_calculation_time graph start_node] runs the breadth-first search algorithm on a graph [graph] starting from a given node [start_node]. 
    It returns a list of sets of nodes, where each set contains all nodes at a certain distance from the starting node. 
    This function runs on a single thread. *)
  let bfs_seq_calculation_time (graph : UnweightedGraph.t) (start_node : Node.t)
      : float =
    let start_time = Unix.gettimeofday () in
    let _ = Bfs.sequential graph start_node in
    (* Printf.printf "Sequential BFS took %f seconds\n" *)
    Unix.gettimeofday () -. start_time

  (* let bfs_par_calculation_time_num_domains_to_csv (graph : UnweightedGraph.t)
      (start_node : Node.t) ~(max_domains : int) : unit =
    let out_channel =
      open_out "computation_time_analysis/bfs_par_domains.csv"
    in
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
    close_out out_channel *)

  let bfs_calculation_time_combinations_to_csv (combinations : (int * int) list)
      ~(task_pool: T.pool) : unit =
    let out_channel =
      open_out "computation_time_analysis/bfs_par_combinations.csv"
    in
    let calculate_and_write (num_nodes, num_edges) =
      let graph =
        UnweightedGraph.create_new_graph ~num_nodes ~num_edges ~directed:false
      in
      let start_node = Option.get (UnweightedGraph.find_node_by_id 1 graph) in
      let parallel_calculation_time =
        bfs_par_calculation_time graph start_node ~task_pool
      in
      let sequential_calculation_time =
        bfs_seq_calculation_time graph start_node
      in
      Printf.fprintf out_channel "%d,%d,%.3f,%.3f\n" num_nodes num_edges
        parallel_calculation_time sequential_calculation_time
    in
    output_string out_channel
      "num_nodes,num_edges,parallel_time,sequential_time\n";
    List.iter calculate_and_write combinations;
    close_out out_channel
end

module BfsPerformanceAnalysis = MakeBfsPerformanceAnalysis (BfsAlgorithms)
(** [BfsPerformanceAnalysis] is a module that provides performance analysis functions for the breadth-first search algorithm. *)
