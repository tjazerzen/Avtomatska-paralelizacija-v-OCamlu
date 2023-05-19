open Graph
open Bfs

let () =
  print_endline
    "Reading graph from file graph_files/undirected_graph_small.txt..."

let new_graph = Graph.graph_from_txt "graph_files/undirected_graph_small.txt"
let node1_option = Graph.find_node_by_id 1 new_graph
let node1 = Option.get node1_option


let () = Printf.printf "Printing node1: %s\n" (Node.to_string node1)

let () =
  List.iter
    (fun node -> Printf.printf "Neighbour: %s\n" (Node.to_string node))
    (Graph.neighbours node1 new_graph)

let () =
  print_endline
    "Running SEQUENTIAL BFS on graph, stored in undirected_graph_small.txt, starting with \
     node with ID equal to 1..."

let () = Bfs.bfs new_graph node1

let () =
  print_endline
    "Running NON-SEQUENTIAL BFS on graph, stored in undirected_graph_small.txt, starting with \
     node with ID equal to 1..."


let _ = Bfs.parallel_bfs3 new_graph node1 4
