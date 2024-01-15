open Graph
open Floyd
open Domainslib

let matrix_to_string (matrix : float array array) =
  let n = Array.length matrix in
  let str = ref "" in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      str := !str ^ string_of_float matrix.(i).(j) ^ " "
    done;
    str := !str ^ "\n"
  done;
  !str

let small_graph_vertex_count = 7
let small_graph_edge_count = 15
let num_domains = 8

let small_graph =
  WeightedGraph.create_new_graph ~num_nodes:small_graph_vertex_count
    ~num_edges:small_graph_edge_count ~directed:false

(* let () = print_endline (WeightedGraph.to_string small_graph) *)

let matrix = FloydWarshallAlgorithms.floyd_warshall_seq small_graph

let () = print_endline ("Sequential: \n" ^ matrix_to_string matrix)
let task_pool = T.setup_pool ~num_domains ()

let matrix_par =
  Task.run task_pool (fun () ->
      FloydWarshallAlgorithms.floyd_warshall_par small_graph task_pool)
;;

print_endline ("Parallel: \n" ^ matrix_to_string matrix_par);
T.teardown_pool task_pool
