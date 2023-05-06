open Unix

(* Define custom types *)
type id = int

(* To think about: maybe need 'a here? *)
type edge = {
  created_time : float;
  src_id : id;
  dest_id : id;
}


(* Helper functions *)
let hash_value (edge: edge) : int =
  int_of_float edge.created_time

let edge_to_string (edge : edge) : string = 
  "source_id: " ^ string_of_int edge.src_id ^ ", destination_id: " ^ string_of_int edge.dest_id ^ "\n"

let create_edge_from_nodes (node1_id : id) (node2_id : id) (is_directed : bool) : (edge option * edge option) = 
  let edge_option_from = Some {created_time = Unix.gettimeofday (); src_id = node1_id; dest_id = node2_id} in
  let edge_option_to = Some {created_time = Unix.gettimeofday (); src_id = node2_id; dest_id = node1_id} in
  match is_directed with
  | true -> (edge_option_from, None)
  | false -> (edge_option_from, edge_option_to)

(*May need to be changed, will see*)
let equal_edge edge1 edge2 =
  edge1.src_id = edge1.src_id && edge2.dest_id = edge2.dest_id