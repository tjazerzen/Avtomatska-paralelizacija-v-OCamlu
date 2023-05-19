open Graph
open Bfs

let print_int_nodemap nodemap = 
  NodeMap.iter (
    fun key value -> 
      Printf.printf "Node: %s - distance from start node: %s\n" (Node.to_string key) (string_of_int value)
    ) nodemap

let graph_file_name = "graph_files/undirected_graph_large.txt"

let () = Printf.printf "Reading graph from file %s...\n" graph_file_name

let new_graph = Graph.graph_from_txt graph_file_name
let node1 = Option.get (Graph.find_node_by_id 0 new_graph)

let () = Printf.printf "Running SEQUENTIAL BFS on graph, stored in %s, \
    starting with node with ID 0...\n" graph_file_name

let levels_nodemap_sequential = Bfs.bfs new_graph node1
let () = print_int_nodemap levels_nodemap_sequential


let () = Printf.printf "Running PARALLEL BFS on graph, stored in %s, \
    starting with node with ID 0...\n" graph_file_name


let levels_nodemap_parallel = Bfs.parallel_bfs new_graph node1 4

let () = print_int_nodemap levels_nodemap_parallel

