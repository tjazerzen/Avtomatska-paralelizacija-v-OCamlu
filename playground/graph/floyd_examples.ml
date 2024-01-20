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

let small_graph_vertex_count = 5
let small_graph_edge_count = 7
let num_domains = 5
let min_factor = 0.3

let small_graph =
  WeightedGraph.create_new_graph ~num_nodes:small_graph_vertex_count
    ~num_edges:small_graph_edge_count ~directed:false

let large_graph_vertex_count = 750

let large_graph_edge_count =
  float_of_int (large_graph_vertex_count * (large_graph_vertex_count - 1) / 2)
  *. min_factor
  |> int_of_float

let large_graph =
  WeightedGraph.create_new_graph ~num_nodes:large_graph_vertex_count
    ~num_edges:large_graph_edge_count ~directed:false

let () =
  Printf.printf "-----------------TEST RUNNING FLOYD WARSHALL----------------\n"

let matrix_seq = FloydWarshallAlgorithms.floyd_warshall_seq small_graph
let () = print_endline ("Sequential: \n" ^ matrix_to_string matrix_seq)
let task_pool = T.setup_pool ~num_domains ()

let matrix_par =
  Task.run task_pool (fun () ->
      FloydWarshallAlgorithms.floyd_warshall_par small_graph task_pool)
;;

print_endline ("Parallel: \n" ^ matrix_to_string matrix_par);
T.teardown_pool task_pool

let () = Printf.printf "-----------------TIME CALCULATIONS----------------\n"

let time_calc_seq =
  FloydWarshallTimeCalculations.time_floyd_warshall_seq large_graph

let () =
  print_endline ("Sequential calculation time: " ^ string_of_float time_calc_seq)

let time_calc_par =
  FloydWarshallTimeCalculations.time_floyd_warshall_par large_graph num_domains

let () =
  print_endline ("Parallel calculation time: " ^ string_of_float time_calc_par)

let () =
  Printf.printf "\n-----------------NUM DOMAINS TO CSV-----------------\n"

let () =
  FloydWarshallAnalysis.par_calc_time_num_domains_to_csv large_graph num_domains

let () = Printf.printf "\n-----------------PAR COMB TO CSV-----------------\n"

let combinations =
  GraphUtils.generate_graph_combinations ~min_vertex:400 ~max_vertex:750
    ~min_factor ~step:50

let () =
  FloydWarshallAnalysis.par_calc_time_combinations_to_csv combinations
    num_domains
