open Graph
open Parany
module T = Domainslib.Task

(** [Queue] is a module that implements a queue data structure using two stacks.
    The first stack is used for enqueueing elements, while the second stack 
    is used for dequeueing elements. *)
module Queue : sig
  type 'a t

  (** [create_empty_queue ()] creates an empty queue. *)
  val create_empty_queue : unit -> 'a t

  (** [is_empty q] returns [true] if the queue [q] is empty, [false] otherwise. *)
  val is_empty : 'a t -> bool

  (** [enqueue x q] adds element [x] to the queue [q]. *)
  val enqueue : 'a -> 'a t -> 'a t

  (** [dequeue q] removes and returns the first element of the queue [q].
    If the queue is empty, returns [None]. *)
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
  val parallel : Graph.t -> Node.t -> NodeSet.t list
  val sequential : Graph.t -> Node.t -> NodeSet.t list
end = struct

  let visit_node (node : Node.t) (visited : NodeSet.t) (graph : Graph.t) : Node.t list =
      let neighbours : Node.t list = Graph.neighbours node graph in
      List.filter (fun node -> not (NodeSet.mem node visited)) neighbours
    
  let rec loop (visited : NodeSet.t) (stages : NodeSet.t list) (next_stage_implementation : NodeSet.t -> NodeSet.t -> Graph.t -> NodeSet.t) (graph : Graph.t) :
          NodeSet.t list =
        match stages with
        | last_stage :: _ ->
            let next : NodeSet.t = next_stage_implementation visited last_stage graph in
            if NodeSet.is_empty next then List.rev stages
            else loop (NodeSet.union visited next) (next :: stages) next_stage_implementation graph
        | [] -> failwith "Should not happen"

  let parallel (graph : Graph.t) (start_node : Node.t) : NodeSet.t list =
    let next_stage_parallel (visited : NodeSet.t) (previous_stage : NodeSet.t) (graph : Graph.t) :
        NodeSet.t =
      let new_nodes : Node.t list =
        previous_stage |> NodeSet.elements
        |> Parmap.parmap 4 (fun node -> visit_node node visited graph)
        |> List.flatten
      in
      List.fold_left
        (fun set node -> NodeSet.add node set)
        NodeSet.empty new_nodes
    in
    loop (NodeSet.singleton start_node) [ NodeSet.singleton start_node ] next_stage_parallel graph
  
  let sequential (graph : Graph.t) (start_node : Node.t) : NodeSet.t list =
    let next_stage_sequential (visited : NodeSet.t) (previous_stage : NodeSet.t) (graph : Graph.t) :
        NodeSet.t =
      let new_nodes : Node.t list =
        previous_stage |> NodeSet.elements
        |> List.map (fun node -> visit_node node visited graph)
        |> List.flatten
      in
      List.fold_left
        (fun set node -> NodeSet.add node set)
        NodeSet.empty new_nodes
    in
    loop (NodeSet.singleton start_node) [ NodeSet.singleton start_node ] next_stage_sequential graph
  
end
