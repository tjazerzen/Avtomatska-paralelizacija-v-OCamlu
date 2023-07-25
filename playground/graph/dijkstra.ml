open Graph

(** This module provides an implementation of Dijkstra's shortest path algorithm for weighted graphs. *)
module Dijkstra : sig
(** The `shortest_path` function computes the shortest path from a given source
    node to all other nodes in the graph, using Dijkstra's algorithm.
    @param graph The weighted graph to compute shortest paths in.
    @param source The source node to compute shortest paths from.
    @return A pair of maps: the first maps each node to its shortest distance
            from the source, and the second maps each node to its predecessor
              in the shortest path from the source. *)
  val shortest_path : WeightedGraph.t -> Node.t -> float NodeMap.t * Node.t NodeMap.t
end = struct
  let shortest_path (graph: WeightedGraph.t) (source: Node.t) =
    let inf = infinity in
    let dist = ref (NodeMap.singleton source 0.) in
    let prev = ref NodeMap.empty in

    let unvisited = ref (NodeSet.of_list (WeightedGraph.nodes graph)) in

    let weight_of_edge node edges =
      let edge = List.find (fun (u, v, _) -> u = node || v = node) edges in
      match edge with
      | (_, _, weight) -> weight
      | exception Not_found -> inf
    in    

    let rec loop () =
      if not (NodeSet.is_empty !unvisited) then
        let u =
          NodeSet.min_elt (NodeSet.filter (fun node -> NodeMap.mem node !dist) !unvisited)
        in
        let neighbours = WeightedGraph.neighbours u graph in
        List.iter
          (fun v ->
            let alt = NodeMap.find u !dist +. (weight_of_edge v (WeightedGraph.edges graph))
            in
            if alt < (try NodeMap.find v !dist with Not_found -> inf) then (
              dist := NodeMap.add v alt !dist;
              prev := NodeMap.add v u !prev))
          neighbours;
        unvisited := NodeSet.remove u !unvisited;
        loop ()
    in
    loop ();
    (!dist, !prev)
end



