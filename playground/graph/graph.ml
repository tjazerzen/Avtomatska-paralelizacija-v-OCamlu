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
  val find_node_by_id : int -> t -> Node.t option
  val create_new_graph : num_nodes:int -> num_edges:int -> directed:bool -> t
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

  let find_node_by_id id graph =
    let nodes = nodes graph in
    try Some (List.find (fun node -> Node.id node = id) nodes)
    with Not_found -> None

  let create_new_graph ~(num_nodes : int) ~(num_edges : int) ~(directed : bool) : t =
    let graph = empty ~directed in
    let nodes = List.init num_nodes (fun i -> Node.create i) in
    let graph =
      List.fold_left (fun graph node -> add_node node graph) graph nodes
    in
    let rec add_edges graph num_edges =
      if num_edges = 0 then graph
      else
        let node1 = List.nth nodes (Random.int num_nodes) in
        let node2 = List.nth nodes (Random.int num_nodes) in
        if Node.compare node1 node2 <> 0 then
          let graph = add_edge node1 node2 graph in
          add_edges graph (num_edges - 1)
        else add_edges graph num_edges
    in
    let () = Node.reset_ids () in
    add_edges graph num_edges
end
