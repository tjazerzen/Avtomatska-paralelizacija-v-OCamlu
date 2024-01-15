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
      let matrix_to_update = Array.map Array.copy matrix in
      T.parallel_for pool ~start:0 ~finish:(n - 1) ~body:(fun i ->
        T.parallel_for pool ~start:0 ~finish:(n - 1) ~body:(fun j ->
          matrix_to_update.(i).(j) <-
            (if i != j then
                min matrix.(i).(j) (matrix.(i).(k) +. matrix.(k).(j))
              else 0.0);
          );
        );
      Array.iteri (fun i row -> Array.blit row 0 matrix.(i) 0 n) matrix_to_update
    done;
    matrix
end
