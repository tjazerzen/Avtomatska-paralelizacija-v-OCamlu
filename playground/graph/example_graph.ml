#use "graph.ml"

(* To run: #use "example_graph.ml";; *)

let force_retrieve_head (lst : int list) : int =
  match lst with
  | [] -> failwith "The list is empty"
  | x :: _ -> x


let () = 
  Printf.printf "Creating an empty undirected graph...\n";
  let g = create_empty_graph false in

  Printf.printf "Adding nodes A, B, and C to the graph...\n";
  let g = add_node_with_content g "A" in
  let g = add_node_with_content g "B" in
  let g = add_node_with_content g "C" in

  Printf.printf "Number of nodes: %d\n" g.node_count;

  Printf.printf "Node IDs: \n";
  Array.iteri (fun i node_option ->
    match node_option with
    | Some node -> Printf.printf "Node: %s, ID: %d\n" node.value node.id
    | None -> ()
  ) g.nodes;

  Printf.printf "Removing a node with value \"B\" from the graph... \n";
  let node_list_to_remove = find_node_by_value g "B" in
  let node_idx_to_remove = force_retrieve_head node_list_to_remove in
  let () = Printf.printf "Removing node with index %d\n" node_idx_to_remove in
  let node_to_remove = Option.get g.nodes.(node_idx_to_remove) in

  let g = remove_node g node_to_remove in

  let () = Printf.printf "Number of nodes after removal: %d\n" g.node_count in
  
  let () = print_string (graph_to_string g) in

  let () = Printf.printf "Connecting nodes A and C...\n" in
  let g = connect_nodes g 0 1 in
  let () = print_string (graph_to_string g) in

  (*Create full graph with three vertices*)
  let () = Printf.printf "Adding node D and connecting it to node A and C...\n" in
  let g = add_node_with_content g "D" in
  let g = connect_nodes g 0 2 in
  let g = connect_nodes g 1 2 in

  let () = print_string (graph_to_string g) in

  ()
