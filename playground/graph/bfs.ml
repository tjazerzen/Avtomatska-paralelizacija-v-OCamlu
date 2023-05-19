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
  val bfs : Graph.t -> Node.t -> unit
  val parallel_bfs1 : Graph.t -> Node.t -> int -> int NodeMap.t
  val parallel_bfs2 : Graph.t -> Node.t -> int -> int NodeMap.t
  val parallel_bfs3 : Graph.t -> Node.t -> int -> unit
end = struct
  let bfs graph start_node =
    (*Rekurzivna sekvenčna implementacija algoritma BFS*)
    let visited = ref NodeSet.empty in
    let queue = Queue.create_empty_queue () in

    (* Mark the start node as visited and enqueue it *)
    visited := NodeSet.add start_node !visited;
    let queue = Queue.enqueue start_node queue in

    let rec loop q =
      if not (Queue.is_empty q) then
        match Queue.dequeue q with
        | Some (node, next_q) ->
            Printf.printf "Visiting node %s\n" (Node.to_string node);
            let neighbours = Graph.neighbours node graph in
            let new_q =
              List.fold_left
                (fun acc_q neighbour ->
                  if not (NodeSet.mem neighbour !visited) then (
                    visited := NodeSet.add neighbour !visited;
                    Queue.enqueue neighbour acc_q)
                  else acc_q)
                next_q neighbours
            in
            loop new_q
        | None -> ()
    in
    loop queue

    let parallel_bfs1 graph start_node num_domains =
      (*
      Rekurzivna paralelna implementacija algoritma BFS.
      Uporaba Domainslib knjižnice.
      Za uporabo paralelizacije uporabljam Domainslib.Task.parallel_for.
      Napaka, ki jo dobim: Fatal error: exception Stdlib.Effect.Unhandled(Domainslib__Task.Wait(_, _))
      *)
      let visited = ref NodeSet.empty in
      let level = ref (NodeMap.empty) in
      let queue = Queue.create_empty_queue () in
      let queue = ref (Queue.enqueue start_node queue) in
      let visited = ref (NodeSet.add start_node !visited) in
      let level = ref (NodeMap.add start_node 0 !level) in
    
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
      bfs_inner ();
      T.teardown_pool pool;
    
      !level  

    let parallel_bfs2 graph start_node num_domains =
      (*
      Rekurzivna paralelna implementacija algoritma BFS.
      V primerjavi z parallel_bfs1, dodatna uporaba Domainslib.Chan (channel) knjižnice.
      Za razliko od parallel_bfs1, kjer uporabljam Domainslib.Task.parallel_for, 
      tukaj uporabljam direktno definicijo taskov z Domainslib.Task.async, na katere počakam z Domainslib.Task.await.
      Napaka: Fatal error: exception Stdlib.Effect.Unhandled(Domainslib__Task.Wait(_, _))
      *)
      let visited = ref NodeSet.empty in
      let level = ref (NodeMap.empty) in
      let channel = Domainslib.Chan.make_unbounded () in
  
      let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
  
      Domainslib.Chan.send channel (Some start_node);
      visited := NodeSet.add start_node !visited;
      level := NodeMap.add start_node 0 !level;
  
      let rec bfs_inner () =
        match Domainslib.Chan.recv channel with
        | Some node ->
            let neighbours = Graph.neighbours node graph in
            let tasks = List.mapi 
              (fun _ neighbour -> T.async pool (fun _ -> 
                if not (NodeSet.mem neighbour !visited) then begin
                  visited := NodeSet.add neighbour !visited;
                  let parent_level =
                    match NodeMap.find_opt node !level with
                    | Some l -> l
                    | None -> failwith "Unexpected missing parent level in BFS"
                  in
                  level := NodeMap.add neighbour (parent_level + 1) !level;
                  Domainslib.Chan.send channel (Some neighbour)
                end))
              neighbours in
            List.iter (T.await pool) tasks;
            bfs_inner ()
        | None -> ()
      in
      bfs_inner ();
      T.teardown_pool pool;
  
      !level
    
      let parallel_bfs3 graph start_node num_domains =
        (*
        Iterativna implementacija paralelnega algoritma BFS.
        Uporabljam Domainslib.Task.parallel_for.
        Dodatno tu uporabim modul Mutex, ki je zadolžen za to, da med 
        sočasnim izvajanjem ne pride do sočasnega spreminjanja podatkovnih struktur.
        Napaka: Fatal error: exception Stdlib.Effect.Unhandled(Domainslib__Task.Wait(_, _))
        *)
        let visited = ref NodeSet.empty in
        let queue = ref (Queue.create_empty_queue ()) in
        let queue_mutex = Mutex.create () in
        let visited_mutex = Mutex.create () in
    
        (* Mark the start node as visited and enqueue it *)
        Mutex.lock visited_mutex;
        visited := NodeSet.add start_node !visited;
        Mutex.unlock visited_mutex;
    
        Mutex.lock queue_mutex;
        queue := Queue.enqueue start_node !queue;
        Mutex.unlock queue_mutex;
    
        let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
    
        while not (Queue.is_empty !queue) do
          (* Dequeue node *)
          Mutex.lock queue_mutex;
          let (node, queue') = Option.get (Queue.dequeue !queue) in
          queue := queue';
          Mutex.unlock queue_mutex;
          
          Printf.printf "Visiting node %s\n" (Node.to_string node);
    
          let neighbours = Graph.neighbours node graph in
    
          (* Visit each neighbour in parallel *)
          T.parallel_for pool ~start:0 ~finish:(List.length neighbours - 1)
            ~body:(fun i ->
              let neighbour = List.nth neighbours i in
    
              Mutex.lock visited_mutex;
              if not (NodeSet.mem neighbour !visited) then begin
                visited := NodeSet.add neighbour !visited;
                Mutex.unlock visited_mutex;
    
                Mutex.lock queue_mutex;
                queue := Queue.enqueue neighbour !queue;
                Mutex.unlock queue_mutex;
              end else
                Mutex.unlock visited_mutex;
            );
        done;
    
        T.teardown_pool pool;
    
    
end


