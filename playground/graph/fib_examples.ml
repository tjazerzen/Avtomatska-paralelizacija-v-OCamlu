let current_fib_number = 43 in
let current_num_domains = 8 in
let max_num_domains = 25 in
let min_fib_number = 38 in
let max_fib_number = 45 in
let sequential_threshold_upper = 38 in
let sequential_threshold_lower = 35 in


print_endline "Printing 43-th Fibonacci number (non-parallel)...";
print_endline
  (string_of_float
     (Fib.FibNumbersMixin.fib_calculation_time current_fib_number));

print_endline "Printing 43-th Fibonacci number (parallel) with 8 domains...";
print_endline
  (string_of_float
     (Fib.FibNumbersMixin.par_fib_calculation_time
        ~num_domains:current_num_domains ~sequential_threshold:sequential_threshold_upper current_fib_number));

print_endline
  "Printing parallel calculation times for 43-th Fibonacci number to csv with \
   number of domains ranging from 1 to 25... (this should take approx. 15 seconds)";
Fib.FibNumbersMixin.par_calculation_time_num_domains_to_csv
  ~max_domains:max_num_domains ~sequential_threshold:sequential_threshold_upper current_fib_number;

print_endline "Printing parallel calculation times for Fibonacci numbers from 38 to 45 to csv with 8 domains, lower seq threshold equal to 35 and upper equal to 38... (this should take approx. 30 seconds))";
Fib.FibNumbersMixin.par_calculation_time_fib_number_to_csv
  ~num_domains:current_num_domains ~sequential_threshold_lower ~sequential_threshold_upper ~min_n:min_fib_number ~max_n:max_fib_number;
