open Graph

(** This module provides an implementation of Dijkstra's shortest path algorithm for weighted graphs. *)

module IntFloatPair =
  struct
   type t = int * float
   let compare (_,x0) (_,x1) = compare x0 x1
  end

module PairSet = Set.Make(IntFloatPair)

module Dijkstra : sig
  (** [shortest_path src dst graph] returns the shortest path from [src] to [dst] in [graph] 
      along with the total weight of that path. If no path exists, returns [None]. *)
  val dijkstra : WeightedGraph.t -> int -> int -> float
end = struct
  let dijkstra (graph: WeightedGraph.t) (start_id: int) (end_id: int) : float = 
    let nodes = WeightedGraph.nodes graph in
    let n = List.length nodes in
    let dist = Array.make n max_float in
    let node_to_idx = Hashtbl.create n in
    List.iteri (fun i node -> Hashtbl.add node_to_idx (Node.id node) i) nodes;
    let q = ref PairSet.(empty |> add (start_id, 0.)) in
    dist.(Hashtbl.find node_to_idx start_id) <- 0.;
    while not (PairSet.is_empty !q) do
      let (u_id, _) = PairSet.min_elt !q in
      let u_idx = Hashtbl.find node_to_idx u_id in
      q := PairSet.remove (u_id, dist.(u_idx)) !q;
      let u_node = match WeightedGraph.find_node_by_id u_id graph with
        | Some node -> node
        | None -> failwith "Node not found"
      in
      List.iter
        (fun v_node -> 
          let v_id = Node.id v_node in
          let v_idx = Hashtbl.find node_to_idx v_id in
          let weight = 
            let edges = WeightedGraph.edges graph in
            let rec find_weight lst = 
              match lst with
              | [] -> failwith "Edge not found"
              | (u, v, w)::rest -> if u = u_node && v = v_node then w else find_weight rest
            in find_weight edges
          in
          let newdist = dist.(u_idx) +. weight in
          if newdist < dist.(v_idx) then
          begin
            q := PairSet.add (v_id, newdist) !q;
            dist.(v_idx) <- newdist
          end
        )
        (WeightedGraph.neighbours u_node graph)
    done;
    dist.(Hashtbl.find node_to_idx end_id)  
  end