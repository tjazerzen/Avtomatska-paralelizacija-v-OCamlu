open Unix

(* Define custom types *)
type id = int
type direction = To | From | Left | Right

(* To think about: maybe need 'a here? *)
type edge_interface = {
  mutable created_time : float;
  directory : (id * direction) list;
}

(* Helper functions *)
let hash_value edge =
  int_of_float edge.created_time

(*For the given edge interface, return the edge that connects the node under node_id. Returns the other's id
  In case that there is no node with such node_id in the given interface, returns -1 *)
let other edge node_id =
  let filtered = List.filter (fun (link_id, _) -> link_id <> node_id) edge.directory in
  match filtered with
  | (link_id, _) :: _ -> link_id
  | [] -> -1
(*
To and From play a role in case of directed edges. Edge goes "From" one note "To" another   
Left and Right play a role in case of undirected edges. Just to make a distinction, I mark one edge as left and one as right
*)
let string_of_direction = function
  | To -> "To"
  | From -> "From"
  | Left -> "Left"
  | Right -> "Right"

(*Buffers are an efficient way for string manipulation. Note that they are mutable though*)
let edge_description edge =
  let buffer = Buffer.create 16 in
  List.iter (fun (key, value) ->
    Printf.bprintf buffer "\n     id: %d, direction: %s" key (string_of_direction value)
  ) edge.directory;
  Buffer.add_char buffer '\n';
  Buffer.contents buffer

(*Will most likely serve as helper to the  method create_edge_from_nodes*)
let create_edge directory =
  { created_time = Unix.gettimeofday (); directory }

let create_edge_from_nodes node1_id node2_id is_directed =
  let new_directory =
    if is_directed then  (* node1 --> node2 *)
      [(node1_id, From); (node2_id, To)]
    else (*node1 --> node2 and node2 --> node1*)
      [(node1_id, Left); (node2_id, Right)] in
  create_edge new_directory

let equal_edge edge1 edge2 =
  edge1.created_time = edge2.created_time