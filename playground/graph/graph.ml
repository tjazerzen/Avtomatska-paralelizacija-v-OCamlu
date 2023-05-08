#use "edge.ml"
#use "node.ml"

(* To run: #use "graph.ml";; *)

(*Define set of integers*)
module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

let initial_graph_size = 100;


type 'a graph = {
  nodes: ('a node Option.t) Array.t; (*The first 'node_count'-elements of graph.nodes with be equal to 'Some node' where graph.nodes.i = node with index i*)
  adjacency_list: IntSet.t Array.t; (*Array of sets - the i-th element of this array will be set of indexes of neighbours of the i-th node*)
  lookup: ('a, int list) Hashtbl.t; (*graph.lookup is a hash table with node's value as key and values as list of IDs to whom that value belongs*)
  is_directed: bool; (*true, if the graph is directed; false if it isn't*)
  node_count: int;
}

(* Helper function to create an empty graph *)
(*O(1)*)
let create_empty_graph (is_directed : bool) : 'a graph = {
  nodes = Array.init initial_graph_size (fun _ -> None);
  adjacency_list = Array.init initial_graph_size (fun _ -> IntSet.empty);
  lookup = Hashtbl.create initial_graph_size;
  is_directed;
  node_count = 0;
}

(*O(1)*)
let add_node_with_content (graph : 'a graph) (content : 'a) : 'a graph =
  let new_node = create_node graph.node_count content in
  let () = Array.set graph.nodes graph.node_count (Some new_node) in
  let () =
    match Hashtbl.find_opt graph.lookup content with
    | Some ids -> Hashtbl.replace graph.lookup content (new_node.id :: ids)
    | None -> Hashtbl.add graph.lookup content [new_node.id]
  in
  { graph with node_count = graph.node_count + 1 }

(*O(initial_graph_size)*)

let update_node_ids (nodes : ('a node option) Array.t) (removed_id : int) =
  Array.map (fun elem ->
    match elem with
    | None -> None
    | Some n -> if n.id > removed_id then Some { n with id = n.id - 1 } else Some n
  ) nodes

(*O(initial_graph_size)*)
let remove_node (graph : 'a graph) (node_to_remove : 'a node) : 'a graph =
  let remove_ith_element_from_arr arr (i : id) replacement_value = 
    Array.concat [Array.sub arr 0 i; Array.sub arr (i+1) (Array.length arr - (i+2)); [|replacement_value|]]
  in
  let id = node_to_remove.id in
  if node_to_remove.id >= graph.node_count then
    let () = Printf.eprintf "Warning: Node with ID %d is not present in the graph. Node will not be removed.\n" node_to_remove.id in
    graph
  else
    (*Define new nodes and adjacency list*)
    let new_nodes = Array.copy graph.nodes in
    let new_adjacency_list = Array.copy graph.adjacency_list in
    (*Array.iteri modifies in-place*)
    Array.iteri (
      fun i set -> if i <> node_to_remove.id then new_adjacency_list.(i) <- IntSet.remove node_to_remove.id set
    ) new_adjacency_list;

    (*Update the existing nodes*)
    let new_nodes = remove_ith_element_from_arr new_nodes id None in
    let new_nodes = update_node_ids new_nodes id in
    (*Update the existing adjacency list*)
    let new_adjacency_list = remove_ith_element_from_arr new_adjacency_list id IntSet.empty in
    (*Remove the node with that value from the hash table*)
    let () = Hashtbl.remove graph.lookup node_to_remove.value in
    { graph with nodes = new_nodes; adjacency_list = new_adjacency_list; node_count = graph.node_count - 1 }


let connect_nodes (graph : 'a graph) (node1_id : int) (node2_id : int) : 'a graph =
  if node1_id >= graph.node_count || node2_id >= graph.node_count then
    let () = Printf.eprintf "Warning: One or both nodes are not present in the graph. Nodes will not be connected.\n" in
    graph
  else
    let new_adjacency_list = Array.copy graph.adjacency_list in
    new_adjacency_list.(node1_id) <- IntSet.add node2_id graph.adjacency_list.(node1_id);
    if not graph.is_directed then new_adjacency_list.(node2_id) <- IntSet.add node1_id graph.adjacency_list.(node2_id);
    { graph with adjacency_list = new_adjacency_list }
    
let connect_nodes_with_id (graph : 'a graph) (node1_id : int) (node2_id : int) : 'a graph =
  match (graph.nodes.(node1_id), graph.nodes.(node2_id)) with
  | (Some node1, Some node2) -> connect_nodes graph node1.id node2.id
  | (_, _) -> Printf.eprintf "Warning: One or both nodes are not present in the graph. Nodes will not be connected.\n"; graph

let find_node_by_value (graph : 'a graph) (value : 'a) : int list =
  match Hashtbl.find_opt graph.lookup value with
  | Some index_list -> index_list
  | None -> Printf.eprintf "Warning: No nodes with the given value found in the graph.\n"; []

let graph_to_string (graph : 'a graph) : string =
  let buffer = Buffer.create 1024 in
  let () = Buffer.add_string buffer "Nodes:\n" in

  Array.iteri (fun i node_option ->
    match node_option with
    | Some node ->
      let neighbors = IntSet.fold (fun neighbor_id acc -> Printf.sprintf "%d %s" neighbor_id acc) graph.adjacency_list.(node.id) "" in
      let node_string = Printf.sprintf "Node: %s, ID: %d, Neighbors: %s\n" node.value node.id neighbors in
      Buffer.add_string buffer node_string
    | None -> ()
  ) graph.nodes;

  Buffer.contents buffer
  