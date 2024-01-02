open Graph
module T = Domainslib.Task

type priority = float
(** The type [priority] represents the priority of a node in Dijkstra's algorithm. *)

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

  val insert : Node.t -> priority -> t -> t
  (** [insert new_node new_priority pq] returns a new priority queue [pq'] with [new_node] inserted with priority [new_priority]. *)

  val remove_top : t -> t
  (** [remove_top pq] returns a new priority queue [pq'] with the lowest priority element removed. Raises [Queue_is_empty] if [pq] is empty. *)

  val extract : t -> priority * Node.t * t
  (** [extract pq] returns a tuple of the priority, node, and priority queue of the lowest priority element in [pq]. Raises [Queue_is_empty] if [pq] is empty. *)
end = struct
  type priority = float
  type t = Empty | PQNode of priority * Node.t * t * t

  exception Queue_is_empty

  let empty () = Empty
  let is_empty (pq : t) = pq = Empty

  let insert (new_node : Node.t) (new_priority : priority) (pq : t) : t =
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

  let extract (pq : t) : priority * Node.t * t =
    let extract_aux pq =
      match pq with
      | Empty -> raise Queue_is_empty
      | PQNode (priority, elt, _, _) as queue ->
          (priority, elt, remove_top queue)
    in
    let priority, extracted_node, updated_pq = extract_aux pq in
    (priority, extracted_node, updated_pq)
end

module Dijkstra : sig
  val sequential : WeightedGraph.t -> Node.t -> Node.t -> priority * Node.t list
  (** [sequential graph start_node end_node] is a sequential implementation of Dijkstra's algorithm for finding the shortest path between [start_node] and [end_node] in the weighted graph [graph]. It returns a tuple of the shortest distance and the list of nodes in the shortest path. *)

  val parallel :
    WeightedGraph.t -> Node.t -> Node.t -> T.pool -> priority * Node.t list
  (** [parallel graph start_node end_node] is a parallel implementation of Dijkstra's algorithm for finding the shortest path between [start_node] and [end_node] in the weighted graph [graph]. It returns a tuple of the shortest distance and the list of nodes in the shortest path. *)
end = struct
  let loop (graph : WeightedGraph.t) (start_node : Node.t) (end_node : Node.t)
      ?(task_pool : T.pool option) =
    let update_pq pq neighbours current_cost new_visited
        ?(task_pool : T.pool option) =
      match task_pool with
      | None ->
          List.fold_left
            (fun pq (neighbor, weight) ->
              if List.mem neighbor new_visited then pq
              else PQ.insert neighbor (current_cost +. weight) pq)
            pq neighbours
      | Some task_pool ->
          let mutex = Mutex.create () in
          let pq_ref = ref pq in
          T.parallel_for task_pool ~start:0
            ~finish:(List.length neighbours - 1)
            ~body:(fun i ->
              let neighbor, weight = List.nth neighbours i in
              if List.mem neighbor new_visited then ()
              else (
                Mutex.lock mutex;
                pq_ref := PQ.insert neighbor (current_cost +. weight) !pq_ref;
                Mutex.unlock mutex));
          !pq_ref
    in
    let rec loop_inner pq visited =
      if PQ.is_empty pq then raise Not_found
      else
        let current_cost, current_node, pq = PQ.extract pq in
        let new_visited = current_node :: visited in
        if current_node = end_node then (current_cost, new_visited)
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
    loop_inner pq_start []

  let sequential (graph : WeightedGraph.t) start_node end_node =
    loop graph start_node end_node ?task_pool:None

  let parallel (graph : WeightedGraph.t) (start_node : Node.t)
      (end_node : Node.t) (task_pool : T.pool) =
    loop graph start_node end_node ~task_pool
end
