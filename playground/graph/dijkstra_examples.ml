open Dijkstra
open Graph
(*--------------------------PRIORITY QUEUE--------------------------*)
let () = Printf.printf "-----------------PRIORITY QUEUE-----------------\n"

(* Create some nodes *)
let node0 = Node.create 3;;
let node1 = Node.create 1;;
let node2 = Node.create 2;;

let pq = PQ.empty () |> PQ.insert node0 3.0 |> PQ.insert node1 1.0 |> PQ.insert node2 2.0;;

(* Print the priority queue *)

(* Extract the nodes from the priority queue *)



let (_, node, pq) = PQ.extract pq;;
Printf.printf "Extracted %s\n" (Node.to_string node);;

let (_, node, pq) = PQ.extract pq;;
Printf.printf "Extracted %s\n" (Node.to_string node);;

let (_, node, pq) = PQ.extract pq;;
Printf.printf "Extracted %s\n" (Node.to_string node);;

(* Try to extract from an empty queue *)
try
  let (_, node, _) = PQ.extract pq in
  Printf.printf "Extracted %s\n" (Node.to_string node)
with PQ.Queue_is_empty ->
  Printf.printf "Cannot extract from an empty queue\n";;

(*--------------------------DIJKSTRA--------------------------*)
(* let () = Printf.printf "-----------------DIJKSTRA-----------------\n"

let small_graph = 
  WeightedGraph.empty ~directed:false 
  |> WeightedGraph.add_node node0 
  |> WeightedGraph.add_node node1 
  |> WeightedGraph.add_node node2
  |> WeightedGraph.add_edge node0 node1 1.0
  |> WeightedGraph.add_edge node1 node2 2.0
  |> WeightedGraph.add_edge node0 node2 1.9

(* let (path, cost) = Dijkstra.sequential small_graph node0 node2;; *)
let (cost, visited) = Dijkstra.sequential small_graph node0 node2;;

let () = List.iter (fun node -> Printf.printf "%s\n" (Node.to_string node)) visited;;
let () = Printf.printf "Cost: %f\n" cost;;
 *)
