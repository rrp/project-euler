(* https://projecteuler.net/problem=4 *)

open Core.Std ;;

(* 
 * without substrings
 *
let is_palindrome num =
        let str = string_of_int num in
        let rec aux s a b =
                if (b <= a) then 
                       true
                else
                        if (s.[a] = s.[b]) then
                                aux s ((+) a 1) ((-) b 1)
                        else
                                false
                in
        aux (String.lowercase str) 0 ((-) (String.length str) 1)
;; *)

let is_palindrome num = 
        let str = string_of_int num in
        let rev = String.rev str in
        str = rev
;;

let max_pal three_digits = 
        let tuples = 
                List.concat 
                (List.map ~f:(fun a -> 
                List.map ~f:(fun b -> (( * ) a b)) three_digits) three_digits) in
        let palindromes = List.filter ~f:(is_palindrome) tuples in
        List.reduce ~f:(max) palindromes
;;

match max_pal (List.range 100 999) with
| None -> printf "No palindromes found\n"
| Some num -> printf "%d\n" num

