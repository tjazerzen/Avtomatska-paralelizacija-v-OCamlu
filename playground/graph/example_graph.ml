#use "graph.ml"


let () = 
  (*Create an empty undirected graph*)
  let g = create_empty_graph false in

  (*Add nodes to the empty undirected graph*)
  let g = add_node_with_content g "A" in
  let g = add_node_with_content g "B" in
  let g = add_node_with_content g "C" in

  (*Test the method node count*)
  Printf.printf "Number of nodes: %d\n" (node_count g);

  (* Update node IDs in the graph *)
  let updated_nodes = update_node_ids g.nodes 1 in

  (* Print the updated node IDs *)
  Printf.printf "Updated node IDs:\n";
  List.iter (fun node -> Printf.printf "Node: %s, ID: %d\n" node.value node.id) updated_nodes;

  (* Find the node to remove *)
  let node_to_remove = List.find (fun node -> node.value = "B") g.nodes in

  (* Remove the node from the graph *)
  let g = remove_node g node_to_remove in

  (* Test the method node count after removal *)
  Printf.printf "Number of nodes after removal: %d\n" (node_count g);
  List.iter (fun node -> Printf.printf "Node: %s, ID: %d\n" node.value node.id) g.nodes;

  g = connect_nodes_with_id (graph) 0 1

  List.iteri (fun i edges ->
    Printf.printf "Node %d: " i;
    List.iter (fun edge -> Printf.printf "(%d, %d) " edge.source edge.destination) edges;
    Printf.printf "\n";
  ) g.adjacency_list;




