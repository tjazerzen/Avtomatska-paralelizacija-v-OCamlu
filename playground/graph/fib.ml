module T = Domainslib.Task
(** Task-based parallelism with [Domainslib.Task]. *)

(** The signature for a Fibonacci computation module. *)
module type Fib = sig
  val fib : int -> int
  (** [fib n] computes the [n]-th Fibonacci number. *)

  val fib_par : T.pool -> int -> int -> int
  (** [fib_par pool sequential_threshold n] computes the [n]-th Fibonacci number
      using parallel computation with a task pool [pool]. When [n] is less than
      or equal to [sequential_threshold], the function reverts to sequential computation. *)
end

(** A module implementing Fibonacci computations. *)
module FibNumbers : Fib = struct
  let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

  let rec fib_par pool sequential_threshold n =
    if n <= sequential_threshold then fib n
    else
      let a =
        T.async pool (fun _ -> fib_par pool sequential_threshold (n - 1))
      in
      let b =
        T.async pool (fun _ -> fib_par pool sequential_threshold (n - 2))
      in
      T.await pool a + T.await pool b
end

(** The signature for a Fibonacci performance analysis module. *)
module MakeFibonacciPerformanceAnalysis (F : Fib) : sig
  val fib_calculation_time : int -> float
  (** [fib_calculation_time n] computes the time to calculate the [n]-th Fibonacci number. *)

  val par_fib_calculation_time :
    num_domains:int -> sequential_threshold:int -> int -> float
  (** [par_fib_calculation_time ~num_domains ~sequential_threshold n] computes the time to calculate
      the [n]-th Fibonacci number using parallel computation with a number of domains specified by [num_domains]
      and a sequential computation threshold specified by [sequential_threshold]. *)

  val par_calculation_time_num_domains_to_csv :
    max_domains:int -> sequential_threshold:int -> int -> unit
  (** [par_calculation_time_num_domains_to_csv ~max_domains ~sequential_threshold n] records the computation time of
      calculating the [n]-th Fibonacci number for a range of domain numbers up to [max_domains] in a CSV file. *)

  val par_calculation_time_fib_number_to_csv :
    num_domains:int -> min_n:int -> max_n:int -> unit
  (** [par_calculation_time_fib_number_to_csv ~num_domains ~sequential_threshold_lower ~sequential_threshold_upper ~min_n ~max_n]
      records the computation time of calculating the Fibonacci numbers from [min_n] to [max_n] for two different sequential
      computation thresholds specified by [sequential_threshold_lower] and [sequential_threshold_upper] in a CSV file. *)
end = struct
  let fib_calculation_time n =
    let start = Unix.gettimeofday () in
    let _ = F.fib n in
    let stop = Unix.gettimeofday () in
    stop -. start

  let par_fib_calculation_time ~num_domains ~sequential_threshold n =
    let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
    let start = Unix.gettimeofday () in
    T.run pool (fun () ->
        let _ = F.fib_par pool sequential_threshold n in
        ());
    let stop = Unix.gettimeofday () in
    T.teardown_pool pool;
    stop -. start

  let par_calculation_time_num_domains_to_csv ~max_domains ~sequential_threshold
      n =
    let out_channel =
      open_out "computation_time_analysis/fib_par_domains.csv"
    in
    let rec par_calculation_time_num_domains_to_csv_aux num_domains n =
      if num_domains > max_domains then ()
      else
        let calculation_time =
          par_fib_calculation_time ~num_domains ~sequential_threshold n
        in
        Printf.fprintf out_channel "%d,%.3f\n" num_domains calculation_time;
        par_calculation_time_num_domains_to_csv_aux (num_domains + 1) n
    in
    output_string out_channel "num_domains,time\n";
    par_calculation_time_num_domains_to_csv_aux 1 n;
    close_out out_channel

  let par_calculation_time_fib_number_to_csv ~num_domains ~min_n ~max_n =
    let out_channel = open_out "computation_time_analysis/fib_par_n.csv" in
    let rec par_calculation_time_fib_number_to_csv_aux n =
      if n > max_n then ()
      else
        let par_time =
          par_fib_calculation_time ~num_domains ~sequential_threshold:38 n
        in
        let seq_time = fib_calculation_time n in
        Printf.fprintf out_channel "%d,%.3f,%.3f\n" n par_time seq_time;
        par_calculation_time_fib_number_to_csv_aux (n + 1)
    in
    output_string out_channel "fib_number,par_time,seq_time\n";
    par_calculation_time_fib_number_to_csv_aux min_n;
    close_out out_channel
end

module FibonacciPerformanceAnalysis =
  MakeFibonacciPerformanceAnalysis (FibNumbers)
(** A module performing Fibonacci computation performance analysis using [FibNumbers]. *)
