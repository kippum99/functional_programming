(* name: Joo Eun (June) Kim *)
(* login: kooeun *)


(* 1.a. *)
(*
Time complexity: O(log n)
This recursive function has 0 as its base case and calls itself
if the argument n is greater than 0, by doing a floor division by 2 and
passing it in as the argument. Each function call does a constant
amount of work. The time complexity scales with precisely
floor(log_2 n) + constant, giving us O(log n).
*)


(* 1.b. *)
(*
Time complexity: O(n)
This recursive function has 0 as its base case and calls itself if the
argument n is greater than 0. Worst-case time complexity is when n is
odd, since it passes in n - 2, another odd number, as an argument to the
recursive call. Each recursive call does a constant amount of work, so
the time complexity is n / 2 for odd n and log n for even n, giving us 
O(n) as the worst case time complexity.
*)


(* 1.c *)
(*
Time complexity: O((log n)^2)
The recursive function iter gets called (log_2 n) * (1 + log_2 n) + 1
times and does a constant amount of work each time. The end condition 
for the recursion is a >= n, and a gets multiplied by 2 when b >= n. It 
takes 1 + log_2 n calls for b to get from 1 to n, and since b resets to 
1 when a is multiplied by 2, it takes a (log_2 n) * (1 + log_2 n) calls 
to get from 1 to n. The term log_2 n * log_2 n dominates the asymptotic 
time complexity, giving us O((log n)^2).
*)


(* 1.d *)


(* 2.1 *)



