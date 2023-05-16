(*Povzeto po: https://github.com/ocaml-multicore/domainslib/blob/master/test/fib.ml*)
let n = try int_of_string Sys.argv.(1) with _ -> 43

let rec fib n = 
  if n < 2 then 1
  else fib (n-1) + fib (n-2)

let main =
  let start_time = Unix.gettimeofday () in
  let res = fib n in
  let end_time = Unix.gettimeofday () in
  Printf.printf "fib(%d) = %d, time = %.3f seconds\n" n res (end_time -. start_time)

let () = main
