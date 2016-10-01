(* https://projecteuler.net/problem=3 *)

open Core.Std ;;

let factor_candidates x = 
        let rec up_to_sqrt x n = 
                if n*n > x then n - 1 else
                        up_to_sqrt x (n + 1) in
        List.range 2 (up_to_sqrt x 2)
;;

let is_prime x = 
        List.fold ~init:true ~f:(&&) (List.map ~f:(fun a -> x mod a <> 0) (factor_candidates x))
;;

let factors x = 
        List.filter ~f:(fun a-> x mod a = 0) (factor_candidates x)
;;

let rec print_list l =
        match l with
        | [] -> ()
        | hd :: tl -> 
                   print_int(hd); print_string(" ");
                   print_list(tl) ;;

let num = 600851475143 ;;
let f = factors num ;;

printf "Some factors of %d: " num ;;
print_list f ;;
print_newline()  ;;
printf "Prime factors of %d: " num ;;
print_list (List.filter ~f:is_prime f) ;;
print_newline()  ;;
