open Graph
open Dijkstra

let print_shortest_path_result (dist, prev) =
  let print_dist node distance =
    Printf.printf "Node %s is at distance %f from the source\n" (Node.to_string node) distance
  in
  let print_prev node predecessor =
    Printf.printf "Node %s's predecessor is node %s\n" (Node.to_string node) (Node.to_string predecessor)
  in
  NodeMap.iter print_dist dist;
  NodeMap.iter print_prev prev


(*Constants for the small graph*)
let small_graph_start_node_id = 2
let small_graph_end_node_id = 9
let small_graph_vertex_count = 10
let small_graph_edge_count = 10

(*----------------------SMALL GRAPH----------------------*)

(* let () = Printf.printf "----------------------SMALL GRAPH----------------------\n" *)
(* let () =
  Printf.printf "Creating graph with %d vertices and %d edges...\n"
    small_graph_vertex_count small_graph_edge_count *)

let small_graph =
  WeightedGraph.create_new_graph ~num_nodes:small_graph_vertex_count
    ~num_edges:small_graph_edge_count ~directed:false

(* let () = Printf.printf "%s" (WeightedGraph.to_string small_graph) *)

let small_graph_start_node = Option.get (WeightedGraph.find_node_by_id small_graph_start_node_id small_graph)

(* let () = print_endline "\n\nResult of Dijkstra's algorithm:\n" *)

(* let res = Dijkstra.shortest_path small_graph small_graph_start_node *)


(* let () = print_shortest_path_result res *)

let res = Dijkstra.dijkstra small_graph small_graph_start_node_id small_graph_end_node_id

let () = Printf.printf "%f" res


(*----------------------LARGE GRAPH----------------------*)