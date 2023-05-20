open Graph
open Bfs

let print_int_nodemap nodemap =
  NodeMap.iter
    (fun key value ->
      Printf.printf "Node: %s - distance from start node: %s\n"
        (Node.to_string key) (string_of_int value))
    nodemap

let graph_file_name = "graph_files/undirected_small_graph.txt"
let start_node_id = 2
let () = Printf.printf "Reading graph from file %s...\n" graph_file_name
let new_graph = graph_file_name |> Graph.graph_from_txt
let node1 = Option.get (Graph.find_node_by_id start_node_id new_graph)

let () =
  Printf.printf
    "Running SEQUENTIAL BFS on graph, stored in %s, starting with node with ID \
     %d...\n"
    graph_file_name start_node_id

let () = Bfs.bfs_sequential new_graph node1 |> print_int_nodemap

let () =
  Printf.printf
    "Running PARALLEL BFS on graph, stored in %s, starting with node with ID \
     %d...\n"
    graph_file_name start_node_id

let () = Bfs.bfs_parallel new_graph node1 10 |> print_int_nodemap
