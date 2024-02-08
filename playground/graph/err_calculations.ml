open Graph
(* open Domainslib *)
open Bfs

(* open BfsAlgorithms *)

(*Helper methods*)

let num_domains = 8

(*Constants for large graph*)
let large_graph_start_node_id = 2
let large_graph_vertex_count = 6000
let large_graph_edge_count = 1000000

(*---------------LARGE GRAPH-----------------*)
let () =
  for _ = 1 to 10 do
    let large_graph =
      UnweightedGraph.create_new_graph
        ~num_nodes:large_graph_vertex_count
        ~num_edges:large_graph_edge_count
        ~directed:false
    in

    let large_graph_start_node =
      match UnweightedGraph.find_node_by_id large_graph_start_node_id large_graph with
      | Some node -> node
      | None -> failwith "Start node not found"
    in

    let calculation_time =
      BfsPerformanceAnalysis.bfs_par_calculation_time large_graph
        large_graph_start_node num_domains
    in
    Printf.printf "%f" calculation_time;
    print_newline ()
  done
