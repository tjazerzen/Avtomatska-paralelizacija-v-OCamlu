open Graph
open Parany
module T = Domainslib.Task

module Queue : sig
  type 'a t

  val create_empty_queue : unit -> 'a t
  val is_empty : 'a t -> bool
  val enqueue : 'a -> 'a t -> 'a t
  val dequeue : 'a t -> ('a * 'a t) option
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

module Bfs : sig
  val bfs_sequential : Graph.t -> Node.t -> int NodeMap.t
  val bfs_parallel : Graph.t -> Node.t -> NodeSet.t list
end = struct
  let bfs_sequential graph start_node =
    let visited = ref NodeSet.empty in
    let level = ref NodeMap.empty in
    let queue = Queue.create_empty_queue () in

    visited := NodeSet.add start_node !visited;
    level := NodeMap.add start_node 0 !level;
    let queue = Queue.enqueue start_node queue in

    let rec loop q =
      if not (Queue.is_empty q) then
        match Queue.dequeue q with
        | Some (node, next_q) ->
            let neighbours = Graph.neighbours node graph in
            let parent_level = NodeMap.find node !level in
            let new_q =
              List.fold_left
                (fun acc_q neighbour ->
                  if not (NodeSet.mem neighbour !visited) then (
                    visited := NodeSet.add neighbour !visited;
                    level := NodeMap.add neighbour (parent_level + 1) !level;
                    Queue.enqueue neighbour acc_q)
                  else acc_q)
                next_q neighbours
            in
            loop new_q
        | None -> ()
    in
    loop queue;
    !level
  
  let bfs_parallel (graph : Graph.t) (start_node : Node.t) : NodeSet.t list =
    let visit_node (node : Node.t) (visited : NodeSet.t) : Node.t list =
      let neighbours : Node.t list = Graph.neighbours node graph in
      List.filter (fun node -> not (NodeSet.mem node visited)) neighbours
    in

    let next_stage (visited : NodeSet.t) (previous_stage : NodeSet.t) : NodeSet.t =
      let new_nodes : Node.t list =
        previous_stage
        |> NodeSet.elements
        |> Parmap.parmap 4 (fun node -> visit_node node visited)
        |> List.flatten
      in
      List.fold_left
        (fun set node -> NodeSet.add node set)
        NodeSet.empty new_nodes
    in

    let rec loop (visited : NodeSet.t) (stages : NodeSet.t list) : NodeSet.t list =
      match stages with
      | last_stage :: _ ->
          let next : NodeSet.t = next_stage visited last_stage in
          if NodeSet.is_empty next then List.rev stages
          else loop (NodeSet.union visited next) (next :: stages)
      | [] -> failwith "Should not happen"
    in

    loop (NodeSet.singleton start_node) [ NodeSet.singleton start_node ]

end
