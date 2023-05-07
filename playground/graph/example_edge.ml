#use "edge.ml"
(* Usage examples *)

(* Create nodes with IDs *)
let node1_id = 0
let node2_id = 1
let node3_id = 2

(* Create directed and undirected edges *)
let directed_edge1 = create_edge_from_nodes node1_id node2_id true
let undirected_edge1 = create_edge_from_nodes node1_id node3_id false

let () =
  Printf.printf "%s" (edge_to_string directed_edge1);
  Printf.printf "%s" (edge_to_string undirected_edge1);
  (* Compare edges *)
  let edge_comparison = equal_edge directed_edge1 undirected_edge1 in
  Printf.printf "Edges are different: %B\n" edge_comparison
