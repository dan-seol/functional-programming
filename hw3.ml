(* Question 1. *)

let rec common twolists =
  match twolists with
  |([], []) -> []
  |(l1, []) -> []
  |([], l2) -> []
  |(x::xs, y::ys) -> if memberof(x, y::ys) then x::common(xs, remove(x, y::ys)) 
      else common(xs, remove(x, y::ys))
;;

(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)

let rec split l =
  match l with 
  |[] -> ([], [])
  |[x] -> ([x], [])
  |x1::x2::[] -> ([x1], [x2])
  |x1::x2::xs -> (x1::fst (split xs), x2::snd (split xs))
;;

(* Question 3 Here you implement merge. *)

let rec merge twolists =
  match twolists with 
  |([], []) -> []
  |(l1, []) -> l1
  |([], l2) -> l2 
  |(x1::[], x2::[]) -> if x1<x2 then x1::x2::[] else x2::x1::[]
  |(x::xs, y::ys) -> if x<y then x::merge(xs, y::ys) else y::merge(x::xs, ys)
;;

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let rec mergesort l = 
  match l with
  |[] -> []
  |x::[] -> x::[] 
  |l1 -> merge( mergesort(fst (split l1)), mergesort(snd (split l1) ) )
;;
