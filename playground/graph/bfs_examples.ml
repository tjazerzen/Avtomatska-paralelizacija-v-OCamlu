open Graph
open Domainslib
open Bfs

(* open BfsAlgorithms *)

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
let large_graph_vertex_count = 2000
let large_graph_edge_count = 500000

(*---------------SMALL GRAPH-----------------*)

let () =
  Printf.printf "Creating graph with %d vertices and %d edges...\n"
    small_graph_vertex_count small_graph_edge_count

let small_graph =
  UnweightedGraph.create_new_graph ~num_nodes:small_graph_vertex_count
    ~num_edges:small_graph_edge_count ~directed:false

let small_graph_start_node =
  Option.get
    (UnweightedGraph.find_node_by_id small_graph_start_node_id small_graph)

(*Sequential BFS*)
let () =
  info_bfs_calculation_print small_graph_start_node_id ~is_sequential:true

let () =
  BfsAlgorithms.sequential small_graph small_graph_start_node
  |> print_bfs_result

(*Parallel BFS*)
let () =
  info_bfs_calculation_print small_graph_start_node_id ~is_sequential:false

let task_pool = T.setup_pool ~num_domains ()

let result =
  Task.run task_pool (fun () ->
      BfsAlgorithms.parallel small_graph small_graph_start_node task_pool)
;;

T.teardown_pool task_pool

(*---------------LARGE GRAPH-----------------*)
(*Reading the graph*)
(* let large_graph =
     UnweightedGraph.create_new_graph ~num_nodes:large_graph_vertex_count
       ~num_edges:large_graph_edge_count ~directed:false

   let () =
     Printf.printf "Creating graph with %d vertices and %d edges...\n"
       large_graph_vertex_count large_graph_edge_count

   let large_graph_start_node =
     Option.get
       (UnweightedGraph.find_node_by_id large_graph_start_node_id large_graph)

   (*Parallel BFS*)
   let () =
     info_bfs_calculation_print large_graph_start_node_id ~is_sequential:false

   let task_pool = T.setup_pool ~num_domains ()

   let _ =
     Printf.printf "Parallel BFS calculation time: %f\n"
       (BfsPerformanceAnalysis.bfs_par_calculation_time large_graph
         large_graph_start_node ~task_pool);;

   T.teardown_pool task_pool;;

   (*Sequential BFS*)
   let () =
     info_bfs_calculation_print large_graph_start_node_id ~is_sequential:true

   let () =
     Printf.printf "Sequential BFS calculation time: %f\n"
       (BfsPerformanceAnalysis.bfs_seq_calculation_time large_graph
         large_graph_start_node)

   let () =
     Printf.printf
       "Printing parallel BFS performance based on num_domains to csv...\n" *)

(*let () =
  BfsPerformanceAnalysis.bfs_par_calculation_time_num_domains_to_csv large_graph
    large_graph_start_node ~max_domains:8 *)

(*Printing sequential BFS performance to csv...*)
(* This code block defines a list of tuples representing combinations of
   node and edge counts for a graph. The tuples are used to calculate the
   performance of sequential BFS on graphs with varying sizes. *)

(* let vertex_combinations = [
     500;
     1000;
     1500;
     2000;
     2500;
     3000;
     3500;
     4000;
     4500;
     5000;
     5500;
     6000;
     6500;
     7000;
     7500;
     8000;
     8500;
     9000;
     9500;
     10000;
   ]


   let minimizing_factor = 0.01
   let edge_combinations = vertex_combinations
     |> List.map float_of_int
     |> List.map (fun v -> (v *. (v -. 1.0) *. minimizing_factor) /. 2.0)
     |> List.map int_of_float

   let combinations = List.combine vertex_combinations edge_combinations

   let () =
     Printf.printf
       "Printing sequential BFS performance based on number of nodes and edges to \
        csv...\n"

   let () =
     BfsPerformanceAnalysis.bfs_calculation_time_combinations_to_csv combinations
       ~num_domains:8 *)
