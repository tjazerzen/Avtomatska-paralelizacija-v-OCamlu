open Graph
module T = Domainslib.Task

module PQ : sig
  type t

  exception Queue_is_empty

  val empty : unit -> t
  val is_empty : t -> bool
  val insert : Node.t -> float -> t -> t
  val extract : t -> float * Node.t * t
end = struct
  type t = Empty | PQNode of float * Node.t * t * t

  exception Queue_is_empty

  let empty () = Empty
  let is_empty (pq : t) = pq = Empty

  let insert (new_node : Node.t) (new_priority : float) (pq : t) : t =
    let rec insert_aux new_node new_priority pq =
      match pq with
      | Empty -> PQNode (new_priority, new_node, Empty, Empty)
      | PQNode (existing_priority, existing_node, left, right) ->
          if new_priority <= existing_priority then
            PQNode
              ( new_priority,
                new_node,
                insert_aux existing_node existing_priority right,
                left )
          else
            PQNode
              ( existing_priority,
                existing_node,
                insert_aux new_node new_priority right,
                left )
    in
    insert_aux new_node new_priority pq

  let remove_top (pq : t) : t =
    let rec remove_top_aux pq =
      match pq with
      | Empty -> raise Queue_is_empty
      | PQNode (_, _, left, Empty) -> left
      | PQNode (_, _, Empty, right) -> right
      | PQNode
          ( _,
            _,
            (PQNode (left_priority, left_node, _, _) as left),
            (PQNode (right_priority, right_node, _, _) as right) ) ->
          if left_priority <= right_priority then
            PQNode (left_priority, left_node, remove_top_aux left, right)
          else PQNode (right_priority, right_node, left, remove_top_aux right)
    in
    remove_top_aux pq

  let extract (pq : t) : float * Node.t * t =
    let extract_aux pq =
      match pq with
      | Empty -> raise Queue_is_empty
      | PQNode (priority, elt, _, _) as queue ->
          (priority, elt, remove_top queue)
    in
    let priority, extracted_node, updated_pq = extract_aux pq in
    (priority, extracted_node, updated_pq)
end

module type Dijkstra = sig
  val sequential : WeightedGraph.t -> Node.t -> unit
  val parallel : WeightedGraph.t -> Node.t -> T.pool -> unit
  val parallel_with_mutex : WeightedGraph.t -> Node.t -> T.pool -> unit
end

module DijkstraAlgorithms : Dijkstra = struct
  let loop_seq graph start_node =
    let update_pq (pq : PQ.t) (neighbours : (Node.t * float) list)
        (current_cost : float) (new_visited : NodeSet.t) =
      let pq_ref = ref pq in
      let update_queue_for_neighbor (i : int)
          (neighbours_array : (Node.t * float) array) =
        let neighbor, weight = neighbours_array.(i) in
        if not (NodeSet.mem neighbor new_visited) then
          pq_ref := PQ.insert neighbor (current_cost +. weight) !pq_ref
      in
      let neighbours_array = Array.of_list neighbours in
      for i = 0 to Array.length neighbours_array - 1 do
        update_queue_for_neighbor i neighbours_array
      done;
      !pq_ref
    in
    let rec loop_inner (pq : PQ.t) (visited : NodeSet.t) : unit =
      if PQ.is_empty pq then ()
      else
        let current_cost, current_node, pq = PQ.extract pq in
        let new_visited = NodeSet.add current_node visited in
        let neighbours =
          graph |> WeightedGraph.edges |> NodeMap.find current_node
          |> NodeMap.bindings
        in
        let new_pq = update_pq pq neighbours current_cost new_visited in
        loop_inner new_pq new_visited
    in
    let pq_start = PQ.empty () |> PQ.insert start_node 0.0 in
    loop_inner pq_start NodeSet.empty

  let loop_par graph start_node task_pool =
    let update_pq (pq : PQ.t) (neighbours : (Node.t * float) list)
        (current_cost : float) (new_visited : NodeSet.t) =
      let pq_ref = ref pq in
      let update_queue_for_neighbor (i : int)
          (neighbours_array : (Node.t * float) array) =
        let neighbor, weight = neighbours_array.(i) in
        if not (NodeSet.mem neighbor new_visited) then
          pq_ref := PQ.insert neighbor (current_cost +. weight) !pq_ref
      in
      let neighbours_array = Array.of_list neighbours in
      T.parallel_for task_pool ~start:0
        ~finish:(Array.length neighbours_array - 1)
        ~body:(fun i -> update_queue_for_neighbor i neighbours_array);
      !pq_ref
    in
    let rec loop_inner (pq : PQ.t) (visited : NodeSet.t) : unit =
      if PQ.is_empty pq then ()
      else
        let current_cost, current_node, pq = PQ.extract pq in
        let new_visited = NodeSet.add current_node visited in
        let neighbours =
          graph |> WeightedGraph.edges |> NodeMap.find current_node
          |> NodeMap.bindings
        in
        let new_pq = update_pq pq neighbours current_cost new_visited in
        loop_inner new_pq new_visited
    in
    let pq_start = PQ.empty () |> PQ.insert start_node 0.0 in
    loop_inner pq_start NodeSet.empty

  let loop_par_with_mutex graph start_node task_pool =
    let update_pq (pq : PQ.t) (neighbours : (Node.t * float) list)
        (current_cost : float) (new_visited : NodeSet.t) =
      let pq_ref = ref pq in
      let update_queue_for_neighbor (i : int)
          (neighbours_array : (Node.t * float) array) =
        let neighbor, weight = neighbours_array.(i) in
        if not (NodeSet.mem neighbor new_visited) then
          pq_ref := PQ.insert neighbor (current_cost +. weight) !pq_ref
      in
      let neighbours_array = Array.of_list neighbours in
      let mutex = Mutex.create () in
      T.parallel_for task_pool ~start:0
        ~finish:(Array.length neighbours_array - 1)
        ~body:(fun i -> 
          Mutex.lock mutex;
          update_queue_for_neighbor i neighbours_array;
          Mutex.unlock mutex
        );
      !pq_ref
    in
    let rec loop_inner (pq : PQ.t) (visited : NodeSet.t) : unit =
      if PQ.is_empty pq then ()
      else
        let current_cost, current_node, pq = PQ.extract pq in
        let new_visited = NodeSet.add current_node visited in
        let neighbours =
          graph |> WeightedGraph.edges |> NodeMap.find current_node
          |> NodeMap.bindings
        in
        let new_pq = update_pq pq neighbours current_cost new_visited in
        loop_inner new_pq new_visited
    in
    let pq_start = PQ.empty () |> PQ.insert start_node 0.0 in
    loop_inner pq_start NodeSet.empty

  let sequential (graph : WeightedGraph.t) start_node =
    loop_seq graph start_node

  let parallel (graph : WeightedGraph.t) (start_node : Node.t)
      (task_pool : T.pool) =
    loop_par graph start_node task_pool
  
  let parallel_with_mutex (graph : WeightedGraph.t) (start_node : Node.t)
      (task_pool : T.pool) =
    loop_par_with_mutex graph start_node task_pool
end


module MakeDijkstraPerformanceAnalysis (Dijkstra : Dijkstra) : sig
  val par_time : WeightedGraph.t -> Node.t -> int -> (WeightedGraph.t -> Node.t -> T.pool -> unit) -> float

  val par_time_regular : WeightedGraph.t -> Node.t -> int -> float
  val par_time_with_mutex : WeightedGraph.t -> Node.t -> int -> float
  
  val seq_time : WeightedGraph.t -> Node.t -> float

  val par_calc_time_num_domains_to_csv :
    WeightedGraph.t -> Node.t -> max_domains:int -> unit

  val par_calc_time_combinations_to_csv : (int * int) list -> int -> unit
end = struct
  let par_time graph start_node num_threads par_method =
    let task_pool = T.setup_pool ~num_domains:(num_threads - 1) () in
    let start_time = Unix.gettimeofday () in
    let () =
      T.run task_pool (fun () -> par_method graph start_node task_pool)
    in
    let end_time = Unix.gettimeofday () in
    T.teardown_pool task_pool;
    end_time -. start_time
   
  let par_time_regular graph start_node num_threads =
    par_time graph start_node num_threads Dijkstra.parallel
   
  let par_time_with_mutex graph start_node num_threads =
    par_time graph start_node num_threads Dijkstra.parallel_with_mutex

  let seq_time graph start_node =
    let start_time = Unix.gettimeofday () in
    let _ = Dijkstra.sequential graph start_node in
    let end_time = Unix.gettimeofday () in
    end_time -. start_time

  let par_calc_time_num_domains_to_csv 
      (graph : WeightedGraph.t)
      (start_node : Node.t) 
      ~(max_domains : int) : unit =
    let out_channel = open_out "computation_time_analysis/dijkstra_par_domains.csv"
    in
    let rec par_calc_time_num_domains_to_csv_aux num_domains =
      if num_domains > max_domains then ()
      else
        let time_par_regular = par_time_regular graph start_node num_domains in
        Printf.fprintf out_channel "%d,regular,%.3f\n" num_domains time_par_regular;
        let time_par_mutex = par_time_with_mutex graph start_node num_domains in
        Printf.fprintf out_channel "%d,with_mutex,%.3f\n" num_domains time_par_mutex;
        par_calc_time_num_domains_to_csv_aux (num_domains + 1)
    in
    output_string out_channel "num_domains,par_type,time\n";
    par_calc_time_num_domains_to_csv_aux 1;
    close_out out_channel

  let par_calc_time_combinations_to_csv 
      (combinations : (int * int) list)
      (num_domains : int) : unit =
    let out_channel =
      open_out "computation_time_analysis/dijkstra_par_combinations.csv"
    in
    let calculate_and_write (num_nodes, num_edges) =
      let graph =
        WeightedGraph.create_new_graph ~num_nodes ~num_edges ~directed:false
      in
      let start_node = Option.get (WeightedGraph.find_node_by_id 1 graph) in
      let par_time_regular = par_time_regular graph start_node num_domains in
      let par_time_with_mutex = par_time_with_mutex graph start_node num_domains in
      let seq_time = seq_time graph start_node in
      Printf.fprintf out_channel "%d,%d,%.3f,%.3f,%.3f\n" num_nodes num_edges
        par_time_regular par_time_with_mutex seq_time
    in
    output_string out_channel "num_nodes,num_edges,par_time_regular,par_time_with_mutex,seq_time\n";
    List.iter calculate_and_write combinations;
    close_out out_channel
end

module DijkstraPerformanceAnalysis =
  MakeDijkstraPerformanceAnalysis (DijkstraAlgorithms)
