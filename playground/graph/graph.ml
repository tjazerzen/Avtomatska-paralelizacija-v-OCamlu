#use "edge.ml"
#use "node.ml"

type 'a graph = {
  nodes: 'a node list;
  adjacency_list: edge list list;
  lookup: ('a, int list) Hashtbl.t;
  is_directed: bool;
}

(* Helper function to create an empty graph *)
let create_empty_graph (is_directed : bool) : 'a graph = {
  nodes = [];
  adjacency_list = [];
  lookup = Hashtbl.create 16;
  is_directed;
}

let node_count (graph : 'a graph) : int = List.length graph.nodes

(*Add node to existing graph with value content
   When adding node, an entirely new graph structure is created*)
let add_node_with_content (graph : 'a graph) (content : 'a) : 'a graph =
  (*Define new adjacency list*)
  let new_adjacency_list = [] :: graph.adjacency_list in
  (*Create new node with incremental ID and *)
  let new_node = create_node (node_count graph) content in
  let new_nodes = new_node :: graph.nodes in
  (*graph.lookup is a hash table with ids as keys and ids of its neighbours as values*)
  let () =
    match Hashtbl.find_opt graph.lookup content with
    | Some ids -> Hashtbl.replace graph.lookup content (new_node.id :: ids)
    | None -> Hashtbl.add graph.lookup content [new_node.id]
  in
  (*Create the same graph but with updated nodes - new nodes; and updated adjacency list - new_adjacency_list *)
  { graph with nodes = new_nodes; adjacency_list = new_adjacency_list }

(*Not sure if I'll have to go more general that removed_id may be of any type*)
let update_node_ids (nodes : 'a node List.t) (removed_id : 'b) : 'a node List.t =
  List.map (
    fun node -> (if node.id > removed_id then { node with id = node.id - 1 } else node)
    ) nodes

(*TODO: Stayed here*)
(*Remove the node from graph*)
let remove_node (graph : 'a graph) (node_to_remove : 'a node) : 'a graph =
  (*Remove the node from the nodes list*)
  let new_nodes = List.filter (fun node -> node.id <> node_to_remove.id) graph.nodes in
  (*Remove the corresponding adjacency list entry for the removed node*)
  let new_adjacency_list = List.filteri (fun i _ -> i <> node_to_remove.id) graph.adjacency_list in
  (*Update the adjacency list to remove any edges that were connected to the removed node*)
  let updated_adjacency_list = List.map (fun edges ->
    List.filter (
      fun edge -> not (List.mem_assoc node_to_remove.id edge.directory)
      ) edges
  ) new_adjacency_list in
  (*Update the node IDs in the nodes list*)
  let updated_nodes = update_node_ids new_nodes node_to_remove.id in
  (*Reconstruct the graph with the updated nodes list and adjacency_list*)
  let empty_graph = create_empty_graph graph.is_directed in
  let new_graph = List.fold_left add_node_with_content empty_graph (
    List.map (fun node -> node.value) updated_nodes
    ) in
  { new_graph with adjacency_list = updated_adjacency_list }
  
    
(* Connect two nodes in the graph *)
let connect_nodes (graph : 'a graph) (node1 : 'a node) (node2 : 'a node) : 'a graph =
  (* Check if either of the nodes is not present in the graph *)
  if not (List.mem node1 graph.nodes) || not (List.mem node2 graph.nodes) then
    (* If either node is not present, return the graph unmodified *)
    graph
  else
    (* Create an edge between the two nodes *)
    let edge = create_edge_from_nodes node1.id node2.id graph.is_directed in
    (* Update the adjacency list to include the new edge *)
    let new_adjacency_list = List.mapi (fun i edges ->
      if i = node1.id then
        (* If the current index is equal to node1's ID, add the edge to the list of edges *)
        edge :: edges
      else if not graph.is_directed && i = node2.id then
        (* If the graph is undirected and the current index is equal to node2's ID, add the edge to the list of edges *)
        edge :: edges
      else
        (* Otherwise *)
        edges
      ) graph.adjacency_list in
    { graph with adjacency_list = new_adjacency_list }

let find_node_by_id (graph : 'a graph) (id : int) : 'a node option =
  try
    Some (List.find (fun node -> node.id = id) graph.nodes)
  with Not_found ->
    None
    
let connect_nodes_with_id (graph : 'a graph) (node1_id : int) (node2_id : int) : 'a graph =
  match ((find_node_by_id graph node1_id), (find_node_by_id graph node1_id)) with
  | (Some node1, Some node2) -> connect_nodes graph node1 node2
  | (_, _) -> graph