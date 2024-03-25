open Dijkstra
open Graph
open Domainslib

(*--------------------------PRIORITY QUEUE--------------------------*)
let () = Printf.printf "-----------------PRIORITY QUEUE-----------------\n"

let extract_and_print pq =
  let priority, node, pq = PQ.extract pq in
  Printf.printf "Extracted %s with priority %f\n" (Node.to_string node) priority;
  pq

(* Create some nodes *)

(* Create some nodes *)
let node0 = Node.create 3
let node1 = Node.create 1
let node2 = Node.create 2
let node3 = Node.create 4

(* Create a priority queue *)

let pq =
  PQ.empty () |> PQ.insert node0 3.0 |> PQ.insert node1 1.0
  |> PQ.insert node2 2.0 |> PQ.insert node3 0.5

(* Print the priority queue *)

let pq = extract_and_print pq
let pq = extract_and_print pq
let pq = extract_and_print pq
let pq = extract_and_print pq;;

(* Try to extract from an empty queue *)

try
  let _ = extract_and_print pq in
  ()
with PQ.Queue_is_empty -> Printf.printf "Cannot extract from an empty queue\n"

(*--------------------------DIJKSTRA--------------------------*)

let () =
  Printf.printf "-----------------DIJKSTRA (on smaller graph)----------------\n"

let small_graph =
  WeightedGraph.empty ~directed:false
  |> WeightedGraph.add_node node0
  |> WeightedGraph.add_node node1
  |> WeightedGraph.add_node node2
  |> WeightedGraph.add_node node3
  |> WeightedGraph.add_edge node0 node1 1.0
  |> WeightedGraph.add_edge node1 node2 2.0
  |> WeightedGraph.add_edge node0 node2 1.9
  |> WeightedGraph.add_edge node2 node3 1.9
  |> WeightedGraph.add_edge node0 node3 3.9

let () = Node.reset_ids ()

let () =
  Printf.printf "Sequential Dijkstra...\n";
  DijkstraAlgorithms.sequential small_graph node0;
  Printf.printf "Parallel Dijkstra...\n\n"

let task_pool = T.setup_pool ~num_domains:6 ()

let () =
  Task.run task_pool (fun () ->
      DijkstraAlgorithms.parallel small_graph node0 task_pool)
;;

Task.teardown_pool task_pool

let () = Printf.printf "Paralell Dijkstra with mutex...\n\n"
let task_pool = T.setup_pool ~num_domains:6 ()

let () =
  Task.run task_pool (fun () ->
      DijkstraAlgorithms.parallel_with_mutex small_graph node0 task_pool)

let () = Task.teardown_pool task_pool;;

Printf.printf "-----------------DIJKSTRA (on larger graph)-----------------\n"

let num_domains = 3
let min_factor = 0.3
let large_graph_start_node_id = 2
let large_graph_vertex_count = 700

let large_graph_edge_count =
  float_of_int (large_graph_vertex_count * (large_graph_vertex_count - 1) / 2)
  *. min_factor
  |> int_of_float

let large_graph =
  WeightedGraph.create_new_graph ~num_nodes:large_graph_vertex_count
    ~num_edges:large_graph_edge_count ~directed:false

let large_graph_start_node =
  Option.get
    (WeightedGraph.find_node_by_id large_graph_start_node_id large_graph)

let info_dijkstra_print ~(is_sequential : bool) =
  Printf.printf "Running %s Dijkstra on graph \n"
    (if is_sequential then "SEQUENTIAL" else "PARALLEL")
;;

info_dijkstra_print ~is_sequential:true

let () = DijkstraAlgorithms.sequential large_graph large_graph_start_node
let () = Printf.printf "\n-----------------TIME CALCULATIONS-----------------\n"

let par_calc_time_regular =
  DijkstraPerformanceAnalysis.par_time_regular large_graph
    large_graph_start_node num_domains
;;

Printf.printf "Parallel time (regular): %f\n" par_calc_time_regular

let par_calc_time_atomic =
  DijkstraPerformanceAnalysis.par_time_with_mutex large_graph
    large_graph_start_node num_domains
;;

Printf.printf "Parallel time (mutex): %f\n" par_calc_time_atomic

let seq_calc_time =
  DijkstraPerformanceAnalysis.seq_time large_graph large_graph_start_node
;;

Printf.printf "Sequential time: %f\n" seq_calc_time

let () =
  Printf.printf "\n-----------------NUM DOMAINS TO CSV-----------------\n"

(* let () =
   DijkstraPerformanceAnalysis.par_calc_time_num_domains_to_csv large_graph
   large_graph_start_node ~max_domains:7 *)

(* let () = Printf.printf "\n-----------------PAR COMB TO CSV-----------------\n"

   let combinations =
     GraphUtils.generate_graph_combinations ~min_vertex:600 ~max_vertex:1000
       ~min_factor ~step:50

   let () =
     DijkstraPerformanceAnalysis.par_calc_time_combinations_to_csv combinations
       num_domains *)
