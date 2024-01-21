open Graph
module T = Domainslib.Task

module type FloydWarshall = sig
  val floyd_warshall_seq : WeightedGraph.t -> float array array
  val floyd_warshall_par : WeightedGraph.t -> T.pool -> float array array
end

module FloydWarshallAlgorithms : FloydWarshall = struct
  let init_matrix (graph : WeightedGraph.t) =
    let nodes = WeightedGraph.nodes graph in
    let n = List.length nodes in
    let matrix = Array.make_matrix n n infinity in
    nodes
    |> List.iter (fun node_from ->
           WeightedGraph.neighbours node_from graph
           |> List.iter (fun (node_to, edge_weight) ->
                  matrix.(Node.id node_from).(Node.id node_to) <- edge_weight));
    matrix |> Array.iteri (fun i row -> row.(i) <- 0.0);
    matrix

  let update_distance matrix i j k =
    matrix.(i).(j) <- min matrix.(i).(j) (matrix.(i).(k) +. matrix.(k).(j))

  let floyd_warshall_seq (graph : WeightedGraph.t) =
    let matrix = init_matrix graph in
    let n = Array.length matrix in
    for k = 0 to n - 1 do
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          update_distance matrix i j k
        done
      done
    done;
    matrix

  let floyd_warshall_par (graph : WeightedGraph.t) (pool : T.pool) :
      float array array =
    let matrix = init_matrix graph in
    let n = Array.length matrix in
    for k = 0 to n - 1 do
      T.parallel_for pool ~start:0 ~finish:(n - 1) ~body:(fun i ->
          for j = 0 to n - 1 do
            update_distance matrix i j k
          done)
    done;
    matrix
end

module type FloydWarshallTimeCalculationsType = sig
  val time_floyd_warshall_seq : WeightedGraph.t -> float
  val time_floyd_warshall_par : WeightedGraph.t -> int -> float
end

module MakeFloydWarshallTimeCalculations (F : FloydWarshall) :
  FloydWarshallTimeCalculationsType = struct
  let time_floyd_warshall_seq (graph : WeightedGraph.t) =
    let start = Unix.gettimeofday () in
    let _ = F.floyd_warshall_seq graph in
    let stop = Unix.gettimeofday () in
    stop -. start

  let time_floyd_warshall_par (graph : WeightedGraph.t) (num_domains : int) =
    let task_pool = T.setup_pool ~num_domains:(num_domains-1) () in
    let start = Unix.gettimeofday () in
    let _ = T.run task_pool (fun () -> F.floyd_warshall_par graph task_pool) in
    let calc_time = Unix.gettimeofday () -. start in
    T.teardown_pool task_pool;
    calc_time
end

module FloydWarshallTimeCalculations =
  MakeFloydWarshallTimeCalculations (FloydWarshallAlgorithms)

module type MakeFloydWarshallAnalysisType = sig
  val par_calc_time_num_domains_to_csv : WeightedGraph.t -> int -> unit
  val par_calc_time_combinations_to_csv : (int * int) list -> int -> unit
end

module MakeFloydWarshallAnalysis (F : FloydWarshallTimeCalculationsType) :
  MakeFloydWarshallAnalysisType = struct
  let par_calc_time_num_domains_to_csv (graph : WeightedGraph.t)
      (max_domains : int) : unit =
    let out_channel =
      open_out "computation_time_analysis/floyd_par_domains.csv"
    in
    let rec par_calc_time_num_domains_to_csv_aux (num_domains : int) : unit =
      if num_domains > max_domains then ()
      else
        let calc_time = F.time_floyd_warshall_par graph num_domains in
        Printf.fprintf out_channel "%d,%f\n" num_domains calc_time;
        par_calc_time_num_domains_to_csv_aux (num_domains + 1)
    in
    output_string out_channel "num_domains,time\n";
    par_calc_time_num_domains_to_csv_aux 1;
    close_out out_channel

  let par_calc_time_combinations_to_csv (combinations : (int * int) list)
      (num_domains : int) : unit =
    let out_channel =
      open_out "computation_time_analysis/floyd_par_combinations.csv"
    in
    let rec par_calc_time_combinations_to_csv_aux
        (combinations : (int * int) list) : unit =
      match combinations with
      | [] -> ()
      | (num_nodes, num_edges) :: tail ->
          let graph =
            WeightedGraph.create_new_graph ~num_nodes ~num_edges ~directed:false
          in
          let time_par = F.time_floyd_warshall_par graph num_domains in
          let time_seq = F.time_floyd_warshall_seq graph in
          Printf.fprintf out_channel "%d,%d,%.3f,%.3f\n" num_nodes num_edges
            time_par time_seq;
          par_calc_time_combinations_to_csv_aux tail
    in
    output_string out_channel "num_nodes,num_edges,par_time,seq_time\n";
    par_calc_time_combinations_to_csv_aux combinations;
    close_out out_channel
end

module FloydWarshallAnalysis =
  MakeFloydWarshallAnalysis (FloydWarshallTimeCalculations)
