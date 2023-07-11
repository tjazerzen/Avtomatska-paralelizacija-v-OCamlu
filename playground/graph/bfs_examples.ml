open Graph
open Bfs

(*Helper methods*)
let print_bfs_sequential_result nodemap =
  NodeMap.iter
    (fun key value ->
      Printf.printf "Node: %s - distance from start node: %s\n"
        (Node.to_string key) (string_of_int value))
    nodemap

let print_bfs_parallel_result result =
  result
  |> List.iteri (fun level nodes ->
          Printf.printf "Level %d:\n" level;
          NodeSet.iter (fun node -> Printf.printf "  Node: %s\n" (Node.to_string node)) nodes;
          print_newline ())

let info_graph_reading_print (graph_file_name : string) =
  Printf.printf "Reading graph from file %s...\n\n" graph_file_name

let info_bfs_calculation_print (is_sequential : bool) (start_node_id : int) (graph_file_name : string) =
  Printf.printf "Running %s BFS on graph, stored in %s, starting with node with ID %d...\n"
    (if is_sequential then "SEQUENTIAL" else "PARALLEL") graph_file_name start_node_id

(*---------------SMALL GRAPH-----------------*)
(*Reading the graph*)
let small_graph_file_name = "graph_files/undirected_small_graph.txt"
let small_graph_start_node_id = 2
let () = info_graph_reading_print small_graph_file_name
let small_graph = small_graph_file_name |> Graph.graph_from_txt
let small_graph_start_node = Option.get (Graph.find_node_by_id small_graph_start_node_id small_graph)

(*Sequential BFS*)
let () = info_bfs_calculation_print true small_graph_start_node_id small_graph_file_name
let () = Bfs.bfs_sequential small_graph small_graph_start_node |> print_bfs_sequential_result

(*Parallel BFS*)
let () = info_bfs_calculation_print false small_graph_start_node_id small_graph_file_name
let () = Bfs.bfs_parallel small_graph small_graph_start_node |> print_bfs_parallel_result

(*---------------LARGE GRAPH-----------------*)
(*Reading the graph*)
let large_graph_file_name = "graph_files/undirected_large_graph.txt"
let large_graph_start_node_id = 2
let () = info_graph_reading_print large_graph_file_name
let large_graph = large_graph_file_name |> Graph.graph_from_txt
let large_graph_start_node = Option.get (Graph.find_node_by_id 1 large_graph)

(*Parallel BFS*)
let () = info_bfs_calculation_print false large_graph_start_node_id large_graph_file_name
let _ = Bfs.bfs_parallel large_graph large_graph_start_node

(*Sequential BFS*)

let () = info_bfs_calculation_print true large_graph_start_node_id large_graph_file_name
let _ = Bfs.bfs_sequential large_graph large_graph_start_node