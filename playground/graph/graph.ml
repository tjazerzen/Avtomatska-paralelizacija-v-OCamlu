module Node : sig
  type elt = int
  type t

  val create : elt -> t
  val value : t -> elt

  (* compare x y returns 0 if x is equal to y, a negative integer if x is less than y, and a positive integer if x is greater than y *)
  val compare_ids : t -> t -> int
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
  let compare_ids node1 node2 = Stdlib.compare node1.id node2.id
  let compare node1 node2 = Stdlib.compare node1.value node2.value

  let to_string node =
    Printf.sprintf "Node(id: %d, value: %d)" node.id node.value

  let id node = node.id
  let reset_ids () = counter := 0
end

module NodeSet = Set.Make (Node)
module NodeMap = Map.Make (Node)
module IntMap = Map.Make (Int)

module UnweightedGraph : sig
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

  let create_new_graph ~(num_nodes : int) ~(num_edges : int) ~(directed : bool)
      : t =
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
        if Node.compare_ids node1 node2 <> 0 then
          let graph = add_edge node1 node2 graph in
          add_edges graph (num_edges - 1)
        else add_edges graph num_edges
    in
    let () = Node.reset_ids () in
    add_edges graph num_edges
end

module WeightedGraph : sig
  type elt = int
  type t

  val empty : directed:bool -> t
  (** [empty ~directed] returns an empty graph with the specified directionality. *)

  val add_node : Node.t -> t -> t
  (** [add_node node graph] returns a new graph with [node] added to it. *)

  val remove_node : Node.t -> t -> t
  (** [remove_node node graph] returns a new graph with [node] removed from it. *)

  val add_edge : Node.t -> Node.t -> float -> t -> t
  (** [add_edge node1 node2 weight graph] returns a new graph with an edge between [node1] and [node2] added to it, with the specified [weight]. *)

  val remove_edge : Node.t -> Node.t -> t -> t
  (** [remove_edge node1 node2 graph] returns a new graph with the edge between [node1] and [node2] removed from it. *)

  val nodes : t -> Node.t list
  (** [nodes graph] returns a list of nodes in the graph. *)

  val to_string : t -> string
  (** [to_string graph] returns a string representation of the graph [graph], including a list of its nodes and edges. *)

  val neighbours : Node.t -> t -> Node.t list
  (** [neighbours node graph] returns a list of nodes that are adjacent to [node] in the graph [graph]. *)

  val find_node_by_id : int -> t -> Node.t option
  (** [find_node_by_id id graph] returns the node in [graph] with the specified [id], if it exists. Otherwise, returns [None]. *)

  val create_new_graph : num_nodes:int -> num_edges:int -> directed:bool -> t
  (** [create_new_graph ~num_nodes ~num_edges ~directed] returns a new graph with [num_nodes] nodes and [num_edges] edges, with the specified directionality. *)

  val edges : t -> float NodeMap.t NodeMap.t
  (** [edges graph] returns a map of maps representing the edges in the graph [graph]. The outer map maps each node in the graph to a map of its neighbours, where the inner map maps each neighbour to the weight of the edge connecting it to the node. *)
end = struct
  type elt = int
  type t = { edges : float NodeMap.t NodeMap.t; directed : bool }

  let empty ~directed = { edges = NodeMap.empty; directed }
  let edges graph = graph.edges

  let add_node (node : Node.t) (graph : t) : t =
    let edges = NodeMap.add node NodeMap.empty graph.edges in
    { graph with edges }

  let remove_node (node : Node.t) (graph : t) : t =
    let edges = NodeMap.remove node graph.edges in
    let edges = NodeMap.map (NodeMap.remove node) edges in
    { graph with edges }

  let add_edge (src : Node.t) (dst : Node.t) (weight : float) (graph : t) : t =
    let edges =
      NodeMap.update src
        (function
          | Some map -> Some (NodeMap.add dst weight map)
          | None -> Some (NodeMap.singleton dst weight))
        graph.edges
    in
    let edges =
      if graph.directed then edges
      else
        NodeMap.update dst
          (function
            | Some map -> Some (NodeMap.add src weight map)
            | None -> Some (NodeMap.singleton src weight))
          edges
    in
    { graph with edges }

  let remove_edge (src : Node.t) (dst : Node.t) (graph : t) : t =
    let edges =
      NodeMap.update src
        (function
          | Some map -> Some (NodeMap.remove dst map)
          | None -> assert false (* nodes should already exist in the graph *))
        graph.edges
    in
    let edges =
      if graph.directed then edges
      else
        NodeMap.update dst
          (function
            | Some map -> Some (NodeMap.remove src map)
            | None -> assert false (* nodes should already exist in the graph *))
          edges
    in
    { graph with edges }

  let nodes (graph : t) : Node.t list =
    NodeMap.bindings graph.edges |> List.map fst

  let to_string (graph : t) : string =
    let nodes_str =
      nodes graph
      |> List.map (fun node -> Node.to_string node)
      |> String.concat ", "
    in
    let edges_aux (graph : t) : (Node.t * Node.t * float) list =
      graph.edges |> NodeMap.bindings
      |> List.map (fun (node1, nodes) ->
             NodeMap.bindings nodes
             |> List.map (fun (node2, weight) -> (node1, node2, weight)))
      |> List.flatten
    in
    let edges_str =
      edges_aux graph
      |> List.map (fun (src, dst, weight) ->
             Printf.sprintf "(%s -> %s : %f)" (Node.to_string src)
               (Node.to_string dst) weight)
      |> String.concat ", "
    in
    Printf.sprintf "{ nodes: [%s]; edges: [%s] }" nodes_str edges_str

  let neighbours (node : Node.t) (graph : t) : Node.t list =
    try NodeMap.find node graph.edges |> NodeMap.bindings |> List.map fst
    with Not_found -> []

  let find_node_by_id (id : int) (graph : t) : Node.t option =
    nodes graph |> List.find_opt (fun node -> Node.id node = id)

  let create_new_graph ~(num_nodes : int) ~(num_edges : int) ~(directed : bool)
      : t =
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
        if Node.compare_ids node1 node2 <> 0 then
          let graph = add_edge node1 node2 (Random.float 1.0) graph in
          add_edges graph (num_edges - 1)
        else add_edges graph num_edges
    in
    let () = Node.reset_ids () in
    add_edges graph num_edges
end
