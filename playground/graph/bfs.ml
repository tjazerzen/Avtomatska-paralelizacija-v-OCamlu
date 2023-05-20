open Graph

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
  val bfs_parallel : Graph.t -> Node.t -> int -> int NodeMap.t
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
  

    let bfs_parallel graph start_node num_domains =
      let queue = ref ((Queue.create_empty_queue ()) |> Queue.enqueue start_node) in
      let visited = ref (NodeSet.empty |> NodeSet.add start_node) in
      let level = ref (NodeMap.empty |> NodeMap.add start_node 0) in
    
      let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
    
      let rec bfs_inner () =
        if Queue.is_empty !queue then ()
        else 
          match Queue.dequeue !queue with 
          | Some (node, remaining_queue) -> 
              queue := remaining_queue;
              let neighbours = Graph.neighbours node graph in
              T.parallel_for pool ~start:0 ~finish:(List.length neighbours - 1) 
              ~body:(fun i ->
                let neighbour = List.nth neighbours i in
                if not (NodeSet.mem neighbour !visited) then begin
                  visited := NodeSet.add neighbour !visited;
                  let parent_level =
                    match NodeMap.find_opt node !level with
                    | Some l -> l
                    | None -> failwith "Unexpected missing parent level in BFS"
                  in
                  level := NodeMap.add neighbour (parent_level + 1) !level;
                  queue := Queue.enqueue neighbour !queue
                end);
              bfs_inner ()
          | None -> failwith "Unexpected empty queue in BFS" 
      in
      T.run pool (fun () -> bfs_inner ());
      T.teardown_pool pool;
    
      !level  
end


