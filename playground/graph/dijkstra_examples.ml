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

let () = Printf.printf "Sequential Dijkstra...\n"
let cost, visited = Dijkstra.sequential small_graph node0 node3

let () =
  List.iter (fun node -> Printf.printf "%s\n" (Node.to_string node)) visited

let () = Printf.printf "Cost: %f\n" cost
let () = Printf.printf "Parallel Dijkstra...\n"
let () = print_endline ""
let task_pool = T.setup_pool ~num_domains:5 ()

let cost, visited =
  Task.run task_pool (fun () ->
      Dijkstra.parallel small_graph node0 node3 task_pool)
;;

Task.teardown_pool task_pool

let () =
  List.iter (fun node -> Printf.printf "%s\n" (Node.to_string node)) visited

let () = Printf.printf "Cost: %f\n" cost
