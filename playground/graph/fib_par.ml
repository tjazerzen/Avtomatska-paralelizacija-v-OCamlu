(*Povzeto po https://github.com/ocaml-multicore/domainslib/blob/master/test/fib_par.ml*)

let max_num_of_domains = try int_of_string Sys.argv.(1) with _ -> 25
let n = try int_of_string Sys.argv.(2) with _ -> 43

let out_channel = open_out "computation_time_analysis/fib_par.csv"

module T = Domainslib.Task

let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let rec fib_par pool n =
  if n <= 38 then fib n
  else
    (*The T.async function is used to create an asynchronous task*)
    let a = T.async pool (fun _ -> fib_par pool (n - 1)) in
    let b = T.async pool (fun _ -> fib_par pool (n - 2)) in
    (*The T.await function is used to wait for the completion of a task and retrieve its result*)
    T.await pool a + T.await pool b

let rec main num_domains =
  if num_domains > max_num_of_domains then ()
  else
    let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
    let start_time = Unix.gettimeofday () in
    let _ = T.run pool (fun _ -> fib_par pool n) in
    let end_time = Unix.gettimeofday () in
    T.teardown_pool pool;
    Printf.fprintf out_channel "%d,%.3f\n" num_domains (end_time -. start_time);
    main (num_domains + 1)

let () =
  output_string out_channel "num_domains,time\n";
  main 1;
  close_out out_channel
