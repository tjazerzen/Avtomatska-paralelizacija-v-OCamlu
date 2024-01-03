open Graph
module T = Domainslib.Task

(** The module [PQ] provides a priority queue implementation used in Dijkstra's algorithm to keep track of the nodes to visit next. *)
module PQ : sig
  type t
  (** The type [t] represents a priority queue, which is used in Dijkstra's algorithm to keep track of the nodes to visit next. *)

  exception Queue_is_empty
  (** [Queue_is_empty] is raised when attempting to remove an element from an empty priority queue. *)

  val empty : unit -> t
  (** [empty ()] returns an empty priority queue. *)

  val is_empty : t -> bool
  (** [val_is_empty pq] returns [true] if [pq] is empty, and [false] otherwise. *)

  val insert : Node.t -> float -> t -> t
  (** [insert new_node new_priority pq] returns a new priority queue [pq'] with [new_node] inserted with priority [new_priority]. *)

  val remove_top : t -> t
  (** [remove_top pq] returns a new priority queue [pq'] with the lowest priority element removed. Raises [Queue_is_empty] if [pq] is empty. *)

  val extract : t -> float * Node.t * t
  (** [extract pq] returns a tuple of the priority, node, and priority queue of the lowest priority element in [pq]. Raises [Queue_is_empty] if [pq] is empty. *)
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
  val sequential : WeightedGraph.t -> Node.t -> Node.t -> float
  val parallel : WeightedGraph.t -> Node.t -> Node.t -> T.pool -> float
end

module DijkstraAlgorithms : Dijkstra = struct
  let loop (graph : WeightedGraph.t) (start_node : Node.t) (end_node : Node.t)
      ?(task_pool : T.pool option) : float =
    let update_pq (pq : PQ.t) (neighbours : (Node.t * float) list)
        (current_cost : float) (new_visited : NodeSet.t)
        ?(task_pool : T.pool option) =
      let pq_ref = ref pq in
      let update_queue_for_neighbor (i : int)
          (neighbours_array : (Node.t * float) array) =
        let neighbor, weight = neighbours_array.(i) in
        if not (NodeSet.mem neighbor new_visited) then
          pq_ref := PQ.insert neighbor (current_cost +. weight) !pq_ref
      in
      let neighbours_array = Array.of_list neighbours in
      match task_pool with
      | None ->
          for i = 0 to Array.length neighbours_array - 1 do
            update_queue_for_neighbor i neighbours_array
          done;
          !pq_ref
      | Some task_pool ->
          T.parallel_for task_pool ~start:0
            ~finish:(Array.length neighbours_array - 1)
            ~body:(fun i -> update_queue_for_neighbor i neighbours_array);
          !pq_ref
    in
    let rec loop_inner (pq : PQ.t) (visited : NodeSet.t) : float =
      if PQ.is_empty pq then raise Not_found
      else
        let current_cost, current_node, pq = PQ.extract pq in
        let new_visited = NodeSet.add current_node visited in
        if current_node = end_node then current_cost
        else
          let neighbours =
            graph |> WeightedGraph.edges |> NodeMap.find current_node
            |> NodeMap.bindings
          in
          let new_pq =
            update_pq pq neighbours current_cost new_visited ?task_pool
          in
          loop_inner new_pq new_visited
    in
    let pq_start = PQ.empty () |> PQ.insert start_node 0.0 in
    loop_inner pq_start NodeSet.empty

  let sequential (graph : WeightedGraph.t) start_node end_node =
    loop graph start_node end_node ?task_pool:None

  let parallel (graph : WeightedGraph.t) (start_node : Node.t)
      (end_node : Node.t) (task_pool : T.pool) =
    loop graph start_node end_node ~task_pool
end

module MakeDijkstraPerformanceAnalysis (Dijkstra : Dijkstra) : sig
  val par_time : WeightedGraph.t -> Node.t -> Node.t -> int -> float
  val seq_time : WeightedGraph.t -> Node.t -> Node.t -> float
  val par_calc_time_num_domains_to_csv :
    WeightedGraph.t -> Node.t -> Node.t -> max_domains:int -> unit
end = struct
  let par_time graph start_node end_node num_threads =
    let task_pool = T.setup_pool ~num_domains:(num_threads - 1) () in
    let start_time = Unix.gettimeofday () in
    let _ =
      T.run task_pool (fun () ->
          Dijkstra.parallel graph start_node end_node task_pool)
    in
    let end_time = Unix.gettimeofday () in
    T.teardown_pool task_pool;
    end_time -. start_time

  let seq_time graph start_node end_node =
    let start_time = Unix.gettimeofday () in
    let _ = Dijkstra.sequential graph start_node end_node in
    let end_time = Unix.gettimeofday () in
    end_time -. start_time

  let par_calc_time_num_domains_to_csv (graph : WeightedGraph.t)
      (start_node : Node.t) (end_node : Node.t) ~(max_domains : int) : unit =
    let out_channel =
      open_out "computation_time_analysis/dijkstra_par_domains.csv"
    in
    let rec par_calc_time_num_domains_to_csv_aux num_domains =
      if num_domains > max_domains then ()
      else
        let time = par_time graph start_node end_node num_domains in
        Printf.fprintf out_channel "%d,%.3f\n" num_domains time;
        par_calc_time_num_domains_to_csv_aux (num_domains + 1)
    in
    output_string out_channel "num_domains,time\n";
    par_calc_time_num_domains_to_csv_aux 1;
    close_out out_channel
end

module DijkstraPerformanceAnalysis =
  MakeDijkstraPerformanceAnalysis (DijkstraAlgorithms)
