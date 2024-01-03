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

let () = Printf.printf "-----------------DIJKSTRA-----------------\n"

(*The shortest path will be node0 --> node2 --> node3*)
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
let () = Printf.printf "Sequential Dijkstra...\n"
let cost = DijkstraAlgorithms.sequential small_graph node0 node3
let () = Printf.printf "Cost: %f\n" cost
let () = Printf.printf "Parallel Dijkstra...\n"
let () = print_endline ""
let task_pool = T.setup_pool ~num_domains:6 ()

let cost =
  Task.run task_pool (fun () ->
      DijkstraAlgorithms.parallel small_graph node0 node3 task_pool)
;;

Task.teardown_pool task_pool

let () = Printf.printf "Cost: %f\n" cost

let () =
  Printf.printf "-----------------DIJKSTRA (on larger graph)-----------------\n"

let num_domains = 8
let min_factor = 0.1
let large_graph_start_node_id = 2
let large_graph_vertex_count = 5000
let large_graph_end_node_id = large_graph_vertex_count - 1

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

let large_graph_end_node =
  Option.get (WeightedGraph.find_node_by_id large_graph_end_node_id large_graph)

let info_dijkstra_print ~(is_sequential : bool) =
  Printf.printf "Running %s Dijkstra on graph \n"
    (if is_sequential then "SEQUENTIAL" else "PARALLEL")
;;

info_dijkstra_print ~is_sequential:true

let cost =
  DijkstraAlgorithms.sequential large_graph large_graph_start_node
    large_graph_end_node
;;

Printf.printf "Shortest path price: ";;
print_float cost

let () = Printf.printf "\n-----------------TIME CALCULATIONS-----------------\n"

let par_calc_time =
  DijkstraPerformanceAnalysis.par_time large_graph large_graph_start_node
    large_graph_end_node num_domains
;;

Printf.printf "Parallel time: %f\n" par_calc_time

let seq_calc_time =
  DijkstraPerformanceAnalysis.seq_time large_graph large_graph_start_node
    large_graph_end_node
;;

Printf.printf "Sequential time: %f\n" seq_calc_time
