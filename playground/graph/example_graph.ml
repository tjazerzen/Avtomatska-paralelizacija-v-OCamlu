#use "graph.ml"

(* To run: #use "example_graph.ml";; *)

let example_graph () = 
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
  let g = remove_node g node_to_remove in

  Printf.printf "Number of nodes after removal: %d\n" g.node_count;
  Array.iteri (fun i node_option ->
    match node_option with
    | Some node -> Printf.printf "Node: %s, ID: %d\n" node.value node.id
    | None -> ()
  ) g.nodes;

  Printf.printf "Connecting nodes A and C...\n";
  let g = connect_nodes g 0 1 in

  Printf.printf "Adjacency list:\n";
  Array.iteri (fun i intset ->
    Printf.printf "Node %d: " i;
    IntSet.iter (fun neighbor_id -> Printf.printf "%d " neighbor_id) intset;
    Printf.printf "\n";
  ) g.adjacency_list;

  Printf.printf "Adding node D and connecting it to node A...\n";
  let g = add_node_with_content g "D" in
  let g = connect_nodes g 0 2 in

  Printf.printf "Printing graph as string:\n";
  Printf.printf "%s\n" (graph_to_string g)

(* Call the example_graph function *)
let () = example_graph ()
