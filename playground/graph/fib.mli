(* fib.mli *)
module T = Domainslib.Task

module FibNumbers : sig
  val fib : int -> int
  val fib_par : T.pool -> int -> int
end

module FibNumbersMixin : sig
    val fib_calculation_time : int -> float
    val par_fib_calculation_time : num_domains:int -> int -> float
    val par_calculation_time_num_domains_to_csv : max_domains:int -> int -> unit
    val par_calculation_time_fib_number_to_csv : num_domains:int -> min_n:int -> max_n:int -> unit
end
