open Graph

module Bfs : sig
  val bfs : Graph.t -> Node.t -> unit
end = struct
  let bfs graph start_node =
    let visited = ref NodeSet.empty in
    let queue = Queue.create () in

    (* Mark the start node as visited and enqueue it *)
    visited := NodeSet.add start_node !visited;
    Queue.add start_node queue;

    while not (Queue.is_empty queue) do
      let node = Queue.take queue in
      Printf.printf "Visiting node %s\n" (Node.to_string node);

      let neighbours = Graph.neighbours node graph in
      List.iter (fun neighbour ->
        if not (NodeSet.mem neighbour !visited) then begin
          visited := NodeSet.add neighbour !visited;
          Queue.add neighbour queue;
        end
      ) neighbours
    done
end
