open Graph

(* open BfsAlgorithms *)
open Bfs

(*Helper methods*)

let print_bfs_result result =
  result
  |> List.iteri (fun level nodes ->
         Printf.printf "Level %d:\n" level;
         NodeSet.iter
           (fun node -> Printf.printf "  Node: %s\n" (Node.to_string node))
           nodes;
         print_newline ())

let info_bfs_calculation_print (start_node_id : int) ~(is_sequential : bool) =
  Printf.printf "Running %s BFS on graph, starting with node with ID %d...\n"
    (if is_sequential then "SEQUENTIAL" else "PARALLEL")
    start_node_id

let num_domains = 8

(*Constants for small graph*)
let small_graph_start_node_id = 2
let small_graph_vertex_count = 10
let small_graph_edge_count = 10

(*Constants for large graph*)
let large_graph_start_node_id = 2
let large_graph_vertex_count = 1000
let large_graph_edge_count = 500000


(*---------------SMALL GRAPH-----------------*)

let () =
  Printf.printf "Creating graph with %d vertices and %d edges...\n"
    small_graph_vertex_count small_graph_edge_count

let small_graph =
  Graph.create_new_graph ~num_nodes:small_graph_vertex_count
    ~num_edges:small_graph_edge_count ~directed:false

let small_graph_start_node =
  Option.get (Graph.find_node_by_id small_graph_start_node_id small_graph)

(*Sequential BFS*)
let () = info_bfs_calculation_print small_graph_start_node_id ~is_sequential:true
let () =
  BfsAlgorithms.sequential small_graph small_graph_start_node
  |> print_bfs_result

(*Parallel BFS*)
let () = info_bfs_calculation_print small_graph_start_node_id ~is_sequential:false
let () =
  BfsAlgorithms.parallel small_graph small_graph_start_node ~num_domains
  |> print_bfs_result

(*---------------LARGE GRAPH-----------------*)
(*Reading the graph*)
let large_graph = Graph.create_new_graph ~num_nodes:large_graph_vertex_count
                     ~num_edges:large_graph_edge_count ~directed:false
let () = Printf.printf "Creating graph with %d vertices and %d edges...\n"
           large_graph_vertex_count large_graph_edge_count
let large_graph_start_node = Option.get (Graph.find_node_by_id large_graph_start_node_id large_graph)

(*Parallel BFS*)
let () = info_bfs_calculation_print large_graph_start_node_id ~is_sequential:false
let _ =
  BfsPerformanceAnalysis.bfs_par_calculation_time large_graph
    large_graph_start_node num_domains

(*Sequential BFS*)

let () = info_bfs_calculation_print large_graph_start_node_id ~is_sequential:true 
let _ =
  BfsPerformanceAnalysis.bfs_seq_calculation_time large_graph
    large_graph_start_node
