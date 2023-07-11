module Node : sig
  type elt = int
  type t

  val create : elt -> t
  val value : t -> elt
  val compare : t -> t -> int
  val to_string : t -> string
  val id : t -> elt
  val reset_ids : unit -> unit
end = struct
  type elt = int
  type t = { id : int; value : elt }

  let counter = ref 0

  let create value =
    let id = !counter in
    incr counter;
    { id; value }

  let value node = node.value
  let compare node1 node2 = Stdlib.compare node1.id node2.id

  let to_string node =
    Printf.sprintf "Node(id: %d, value: %d)" node.id node.value

  let id node = node.id

  let reset_ids () = counter := 0
end

module NodeSet = Set.Make (Node)
module NodeMap = Map.Make (Node)
module IntMap = Map.Make (Int)

module Graph : sig
  type elt = int (* node values *)
  type t (* graphs *)

  val empty : directed:bool -> t
  val add_node : Node.t -> t -> t
  val remove_node : Node.t -> t -> t
  val add_edge : Node.t -> Node.t -> t -> t
  val remove_edge : Node.t -> Node.t -> t -> t
  val nodes : t -> Node.t list
  val edges : t -> (Node.t * Node.t) list
  val to_string : t -> string
  val neighbours : Node.t -> t -> Node.t list
  val graph_from_txt : string -> t
  val find_node_by_id : int -> t -> Node.t option
end = struct
  type elt = int (*Each node's values will be integers*)

  type t = {
    edges : NodeSet.t NodeMap.t;
        (*edges are represented with a Map whoose keys are of type Node.t and values are NodeSet.t*)
    directed : bool;
  }

  let remove_node node graph =
    let edges =
      graph.edges |> NodeMap.remove node |> NodeMap.map (NodeSet.remove node)
    in
    { graph with edges }

  let add_edge node1 node2 graph =
    let add_directed_edge node1 node2 graph =
      let edges =
        graph.edges
        |> NodeMap.add node1
             (NodeSet.add node2 (NodeMap.find node1 graph.edges))
      in
      { graph with edges }
    in
    if graph.directed then add_directed_edge node1 node2 graph
    else add_directed_edge node1 node2 graph |> add_directed_edge node2 node1

  let remove_edge node1 node2 graph =
    let edges =
      graph.edges
      |> NodeMap.add node1
           (NodeSet.remove node2 (NodeMap.find node1 graph.edges))
    in
    let edges =
      if graph.directed then edges
      else
        edges
        |> NodeMap.add node2
             (NodeSet.remove node1 (NodeMap.find node2 graph.edges))
    in
    { graph with edges }

  let nodes graph = NodeMap.bindings graph.edges |> List.map fst
  let empty ~directed = { edges = NodeMap.empty; directed }

  let add_node node graph =
    let edges = NodeMap.add node NodeSet.empty graph.edges in
    { graph with edges }

  let to_string graph =
    let nodes =
      NodeMap.bindings graph.edges
      |> List.map fst
      |> List.map (fun node -> string_of_int (Node.value node))
      |> String.concat ", " |> Printf.sprintf "[%s]"
    in
    let edges =
      graph.edges |> NodeMap.bindings
      |> List.map (fun (node1, nodes) ->
             let nodes =
               NodeSet.elements nodes
               |> List.map (fun node -> string_of_int (Node.value node))
               |> String.concat ", "
               |> fun nodes -> "[" ^ nodes ^ "]"
             in
             "(" ^ string_of_int (Node.value node1) ^ ", " ^ nodes ^ ")")
      |> String.concat ", "
    in
    let edges = Printf.sprintf "[%s]" edges in
    Printf.sprintf "nodes: %s\nedges: %s" nodes edges

  let edges graph =
    graph.edges |> NodeMap.bindings
    |> List.map (fun (node1, nodes) ->
           NodeSet.fold (fun node2 acc -> (node1, node2) :: acc) nodes [])
    |> List.flatten

  let neighbours node graph = NodeMap.find node graph.edges |> NodeSet.elements

  let graph_from_txt path =
    (*
      Reads a graph from a .txt file. Graph files are going to be stored in folder ./playground/graph/graph_files.
      The example how the file needs to be formatted my be found there.
    *)
    let ic = open_in path in
    let rec read_lines ic =
      try let line = input_line ic in line :: read_lines ic
      with End_of_file -> close_in ic; []
    in
    let lines = read_lines ic in
  
    (* Split the lines into nodes and edges *)
    let rec split_lines acc = function
      | "" :: t -> (List.rev acc, t)
      | h :: t -> split_lines (h :: acc) t
      | [] -> (acc, [])
    in
    let (node_lines, edge_lines) = split_lines [] lines in
  
    (* Create nodes *)
    let create_node line =
      let id, value = Scanf.sscanf line "%d:%d" (fun id value -> (id, value)) in
      let node = Node.create value in
      (id, node)
    in
    let nodes = List.map create_node node_lines in
  
    (* Create a graph with the nodes *)
    let graph = List.fold_left (fun g (_, node) -> add_node node g) (empty ~directed:false) nodes in
  
    (* Convert the nodes list to a map for easier lookup when creating edges *)
    let node_map = List.fold_left (fun map (id, node) -> IntMap.add id node map) IntMap.empty nodes in
  
    (* Add edges *)
    let add_edge line graph =
      let id1, id2 = Scanf.sscanf line "%d-%d" (fun id1 id2 -> (id1, id2)) in
      let node1 = IntMap.find id1 node_map in
      let node2 = IntMap.find id2 node_map in
      add_edge node1 node2 graph
    in
    let () = Node.reset_ids () in
    List.fold_right add_edge edge_lines graph
  
    let find_node_by_id id graph =
      let nodes = nodes graph in
      try Some (List.find (fun node -> Node.id node = id) nodes)
      with Not_found -> None

end
