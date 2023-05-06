#use "edge.ml"

let () = 
  let edge1 = create_edge_from_nodes 1 2 false in
  let edge2 = create_edge_from_nodes 2 3 false in
  let edge3 = create_edge_from_nodes 1 4 false in
  let edge4 = create_edge_from_nodes 4 5 false in
  let edges = [edge1; edge2; edge3; edge4] in
  (*print the edges*)
  List.iter (fun edge -> Printf.printf "%s\n" (edge_description edge)) edges

(*
Prints:
  id: 1, direction: Left
  id: 2, direction: Right

  id: 2, direction: Left
  id: 3, direction: Right

  id: 1, direction: Left
  id: 4, direction: Right

  id: 4, direction: Left
  id: 5, direction: Right   
*)