(* Question 1. *)

let rec pairlists (l1, l2) =
  match (l1, l2) with
  |([],[]) -> [] 
  |(x::xs, y::ys) -> (x,y)::pairlists (xs, ys)
  
  
;;

let wmean weights data =
  match (weights, data) with
  |([], []) -> 0.0
  |(_, _) -> (sumlist (List.map (fun (x,y) -> x*.y) (pairlists(weights, data)))) /.(sumlist weights)
  
;;

(* Question 2. *)

let rec memberof (n, l) =
  match l with
  | [] -> false
  | x::xs -> (n=x)|| memberof(n, xs)
;;

let rec remove (item, lst) =
  
  match lst with
  | [] -> [] 
  | x::xs -> if (not(memberof(item, lst)) ) then lst 
      else if (item=x) then remove(item, xs) 
      else x::remove(item, xs)
;;

(* Question 3. *)

let find_max l =
  let rec helper l =
    match l with 
    | [] -> 0
    | [x] -> x
    | x:: xs -> if (x >= helper xs) then x else (helper xs) 
  in helper l
;;

(* Question 4. *)

let rec selsort l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x::xs -> (find_max l)::selsort (remove((find_max l), l)) 
;;
