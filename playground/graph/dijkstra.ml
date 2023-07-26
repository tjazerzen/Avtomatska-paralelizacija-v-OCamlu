open Graph

module T = Domainslib.Task

type priority = float

module PQ : sig
  type elt
  type t
  exception Queue_is_empty

  val empty : unit -> t
  val insert : Node.t -> priority -> t -> t
  (* val remove_top : t -> t *)
  val extract : t -> (priority * Node.t * t)
end = struct
  type elt = Empty | PQNode of priority * Node.t * elt * elt
  type t = {priority_queue: elt; mutex : Mutex.t}

  exception Queue_is_empty

  let empty () = 
    let pq = {priority_queue = Empty; mutex = Mutex.create ()} in
    (* Mutex.unlock pq.mutex; *)
    pq

  let insert (new_node : Node.t) (new_priority : priority) (pq : t) : t =
    Mutex.lock pq.mutex;
    let rec aux (queue : elt) : elt =
      match queue with
        Empty -> PQNode(new_priority, new_node, Empty, Empty)
      | PQNode(existing_priority, existing_node, left, right) ->
          if new_priority <= existing_priority
          then PQNode(new_priority, new_node, aux right, left)
          else PQNode(existing_priority, existing_node, right, aux left)
    in
    let new_pq = {pq with priority_queue = aux pq.priority_queue} in
    Mutex.unlock pq.mutex;
    new_pq

  let extract (pq : t) : (priority * Node.t * t) =
    let remove_top (pq : t) : t =
      let rec remove_top_aux : elt -> elt = function
          Empty -> raise Queue_is_empty
        | PQNode(_, _, left, Empty) -> left
        | PQNode(_, _, Empty, right) -> right
        | PQNode(_, _, (PQNode(left_priority, left_node, _, _) as left),
                          (PQNode(right_priority, right_node, _, _) as right)) ->
            if left_priority <= right_priority
            then PQNode(left_priority, left_node, remove_top_aux left, right)
            else PQNode(right_priority, right_node, left, remove_top_aux right)
      in
      {pq with priority_queue = remove_top_aux pq.priority_queue}
    in
    Mutex.lock pq.mutex;
    let extracted_priority, result, new_queue = 
      match pq.priority_queue with
        Empty -> raise Queue_is_empty
      | PQNode(priority, elt, _, _) -> (priority, elt, remove_top pq)
    in
    Mutex.unlock pq.mutex;
    extracted_priority, result, new_queue
    
end



(* module Dijkstra : sig
  val sequential : WeightedGraph.t -> Node.t -> Node.t -> (priority * Node.t list)
  val parallel : WeightedGraph.t -> Node.t -> Node.t -> (priority * Node.t list)
end = struct
  let sequential (graph: WeightedGraph.t) start_node end_node =
    let rec loop pq visited =
      if pq = PQ.empty ()
      then raise Not_found
      else
        let (current_cost, current_node, pq) = PQ.extract pq in
        let new_visited = current_node :: visited in
        if current_node = end_node
        then (current_cost, new_visited)
        else
          let new_pq =
            List.fold_left
              (fun pq (neighbor, weight) ->
                if List.mem neighbor new_visited then pq
                else PQ.insert neighbor (current_cost +. weight) pq)
              pq
              (WeightedGraph.edges graph |> NodeMap.find current_node |> NodeMap.bindings)
          in
          loop new_pq new_visited
    in
    let pq_start = PQ.empty () |> PQ.insert start_node 0.0 in
    let (cost, path) = loop pq_start [] in
    (cost, List.rev path)

  let parallel (graph: WeightedGraph.t) start_node end_node =
    let rec loop pq visited =
      if pq = PQ.empty ()
      then raise Not_found
      else
        let (current_cost, current_node, pq) = PQ.extract pq in
        let new_visited = current_node :: visited in
        if current_node = end_node
        then (current_cost, new_visited)
        else
          let neighbours = WeightedGraph.edges graph |> NodeMap.find current_node |> NodeMap.bindings in
          let new_pq = T.setup_pool ~num_domains:(List.length neighbours - 1) () in
          T.parallel_for new_pq ~start:0 ~finish:(List.length neighbours - 1) 
          ~body:(fun i ->
            let (neighbor, weight) = List.nth neighbours i in
            if List.mem neighbor new_visited then pq
            else PQ.insert neighbor (current_cost +. weight) pq
            ()
          );
          T.teardown_pool new_pq;
          loop pq new_visited
    in
    let pq_start = PQ.empty () |> PQ.insert start_node 0.0 in
    let (cost, path) = loop pq_start [] in
    (cost, List.rev path)
end *)
