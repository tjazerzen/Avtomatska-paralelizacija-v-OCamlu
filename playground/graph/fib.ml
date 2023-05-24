module T = Domainslib.Task

module FibNumbers : sig
  val fib : int -> int
  val fib_par : T.pool -> int -> int -> int
end = struct
  let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

  let rec fib_par pool sequential_threshold n =
    if n <= sequential_threshold then fib n
    else
      let a = T.async pool (fun _ -> fib_par pool sequential_threshold (n - 1)) in
      let b = T.async pool (fun _ -> fib_par pool sequential_threshold (n - 2)) in
      T.await pool a + T.await pool b
end

(*
Imenuj drugače kot mixin
naredi funktor   
*)
module FibNumbersMixin : sig
  val fib_calculation_time : int -> float
  val par_fib_calculation_time : num_domains:int -> sequential_threshold:int -> int -> float
  val par_calculation_time_num_domains_to_csv : max_domains:int -> sequential_threshold:int -> int -> unit

  val par_calculation_time_fib_number_to_csv :
    num_domains:int -> sequential_threshold_lower:int -> sequential_threshold_upper:int -> min_n:int -> max_n:int -> unit
end = struct
  let fib_calculation_time n =
    let start = Unix.gettimeofday () in
    let _ = FibNumbers.fib n in
    let stop = Unix.gettimeofday () in
    stop -. start

  let par_fib_calculation_time ~num_domains ~sequential_threshold n =
    let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
    let start = Unix.gettimeofday () in
    T.run pool (fun () ->
        let _ = FibNumbers.fib_par pool sequential_threshold n in
        ());
    T.teardown_pool pool;
    let stop = Unix.gettimeofday () in
    stop -. start

  let par_calculation_time_num_domains_to_csv ~max_domains ~sequential_threshold n =
    let out_channel =
      open_out "computation_time_analysis/fib_par_domains.csv"
    in
    let rec par_calculation_time_num_domains_to_csv_aux num_domains n =
      if num_domains > max_domains then ()
      else
        let calculation_time = par_fib_calculation_time ~num_domains ~sequential_threshold n in
        Printf.fprintf out_channel "%d,%.3f\n" num_domains calculation_time;
        par_calculation_time_num_domains_to_csv_aux (num_domains + 1) n
    in
    output_string out_channel "num_domains,time\n";
    par_calculation_time_num_domains_to_csv_aux 1 n;
    close_out out_channel

  let par_calculation_time_fib_number_to_csv ~num_domains ~sequential_threshold_lower ~sequential_threshold_upper ~min_n ~max_n =
    (* let out_channel = open_out "computation_time_analysis/fib_par_n.csv" in *)
    let out_channel = open_out "computation_time_analysis/fib_par_n.csv" in
    let rec par_calculation_time_fib_number_to_csv_aux n =
      if n > max_n then ()
      else
        let parallel_calculation_time_upper_threshold =
          par_fib_calculation_time ~num_domains ~sequential_threshold:sequential_threshold_upper n
        in
        let parallel_calculation_time_lower_threshold =
          par_fib_calculation_time ~num_domains ~sequential_threshold:sequential_threshold_lower n
        in
        let non_parallel_calculation_time = fib_calculation_time n in
        Printf.fprintf out_channel "%d,%.3f,%.3f,%.3f\n" n
          parallel_calculation_time_lower_threshold parallel_calculation_time_upper_threshold non_parallel_calculation_time;
        par_calculation_time_fib_number_to_csv_aux (n + 1)
    in
    output_string out_channel
      "fib_number,parallel_time_lower_threshold,parallel_time_upper_threshold,non_parallel_time\n";
    par_calculation_time_fib_number_to_csv_aux min_n;
    close_out out_channel
end
