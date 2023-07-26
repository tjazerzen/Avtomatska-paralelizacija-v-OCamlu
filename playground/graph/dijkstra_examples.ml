open Dijkstra
open Graph

(* Create an empty priority queue *)
let pq = PQ.empty ();;

(* Create some nodes *)
let node1 = Node.create 3;;
let node2 = Node.create 1;;
let node3 = Node.create 2;;

let pq = pq |> PQ.insert node1 3.0 |> PQ.insert node2 1.0 |> PQ.insert node3 2.0;;

(* Print the priority queue *)

(* Extract the nodes from the priority queue *)
let (node, pq) = PQ.extract pq;;
Printf.printf "Extracted %s\n" (Node.to_string node);;

let (node, pq) = PQ.extract pq;;
Printf.printf "Extracted %s\n" (Node.to_string node);;

let (node, pq) = PQ.extract pq;;
Printf.printf "Extracted %s\n" (Node.to_string node);;

(* Try to extract from an empty queue *)
try
  let (node, _) = PQ.extract pq in
  Printf.printf "Extracted %s\n" (Node.to_string node)
with PQ.Queue_is_empty ->
  Printf.printf "Cannot extract from an empty queue\n";;
