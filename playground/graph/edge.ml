open Unix

(* Define custom types *)
type id = int

(* To think about: maybe need 'a here? *)
type edge = {
  created_time : float;
  src_id : id;
  dest_id : id;
  is_directed : bool
}


(* Helper functions *)
let hash_value (edge: edge) : int =
  int_of_float edge.created_time

let edge_to_string (edge : edge) : string = 
  Printf.sprintf "%d %s---> %d" edge.src_id (if edge.is_directed then "" else "<") edge.dest_id

(*If the graph is not directed, return a tuple of option edges in both directions
   If the graph is directed, return a tuple where the first element is the option of the edge
in which direction we're going and the second is None*)

let create_edge_from_nodes (node1_id : id) (node2_id : id) (is_directed : bool) : edge = 
  {created_time = Unix.gettimeofday (); src_id = node1_id; dest_id = node2_id; is_directed = is_directed}

(*May need to be changed, will see*)
let equal_edge edge1 edge2 =
  edge1.src_id = edge1.src_id && edge2.dest_id = edge2.dest_id