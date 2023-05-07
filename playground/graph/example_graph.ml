#use "graph.ml"

(* To run: #use "example_graph.ml";; *)

let example_graph () = 
  Printf.printf "Creating an empty undirected graph...\n";
  let g = create_empty_graph false in

  Printf.printf "Adding nodes A, B, and C to the graph...\n";
  let g = add_node_with_content g "A" in
  let g = add_node_with_content g "B" in
  let g = add_node_with_content g "C" in

  Printf.printf "Number of nodes: %d\n" (node_count g);

  Printf.printf "Node IDs: \n";
  List.iter (fun node -> Printf.printf "Node: %s, ID: %d\n" node.value node.id) g.nodes;

  Printf.printf "Updating the node ids - \"removing\" node with id 1... \n";
  let updated_nodes = update_node_ids g.nodes 1 in
  Printf.printf "Updated node IDs:\n";
  List.iter (fun node -> Printf.printf "Node: %s, ID: %d\n" node.value node.id) updated_nodes;

  Printf.printf "Removing a node with value \"B\" from the graph... \n";
  let node_to_remove = List.find (fun node -> node.value = "B") g.nodes in
  let g = remove_node g node_to_remove in

  Printf.printf "Number of nodes after removal: %d\n" (node_count g);
  List.iter (fun node -> Printf.printf "Node: %s, ID: %d\n" node.value node.id) g.nodes;

  Printf.printf "Connecting nodes A and C...\n";
  let g = connect_nodes_with_id g 0 1 in

  Printf.printf "Adjacency list:\n";
  List.iteri (fun i edges ->
    Printf.printf "Node %d: " i;
    List.iter (fun edge -> Printf.printf "(%d, %d) " edge.src_id edge.dest_id) edges;
    Printf.printf "\n";
  ) g.adjacency_list;

  Printf.printf "Adding node D and connecting it to node A...\n";
  let g = add_node_with_content g "D" in
  let g = connect_nodes_with_id g 0 2 in

  Printf.printf "Printing graph as string:\n";
  Printf.printf "%s\n" (graph_to_string g)

(* Call the example_graph function *)
let () = example_graph ()
