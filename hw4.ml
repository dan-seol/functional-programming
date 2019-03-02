(* Question 1 *)

let rec mapTree f (t: 'a tree) =
  match t with
  |Empty -> Empty
  |Node(l, h, r) -> Node((mapTree f l), (f h), (mapTree f r))
;;

(* Question 2. *)

let rec halfint (f, (epsilon : float)) ((negValue : float), (posValue : float)) =
  let t = (negValue+.posValue)/.2.0 in 
  if abs_float((f t)) < epsilon then t
  else if (f t) > 0.0 then (halfint (f, epsilon) (negValue, t))
  else (halfint (f, epsilon) (t, posValue))
;;

(* Question 3. *)

let rec newton (f, (epsilon:float), (dx:float)) (guess:float) =
  let close x y = abs_float (x -. y) < epsilon in
  let improve (guess:float) = (guess -. ((f guess)/.(deriv(f, dx) guess))) in
  if close (f guess) 0.0 then guess
  else newton (f, epsilon, dx) (improve (guess))
;;

(* Question 4. *)

let indIntegral (f, (dx:float)) = 
  fun (x: float) -> (integral(f, 0.0, x, dx))
;;
