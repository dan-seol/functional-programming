(* Question 1.1 *)

let areNeighbours ct1 ct2 (cht : chart) =
  List.exists(fun(x,y)->(x=ct1 && y=ct2)||(x=ct2&&y=ct1)) cht
;;

(* Question 1.2 *)

let canBeExtBy (col:colour) (ct: country) (ch : chart) =
  List.for_all(fun x ->areNeighbours ct x ch=false) col
;;

(* Question 1.3 *)

let rec extColouring (cht: chart) (colours : colouring) (cntry : country) =
  match colours with
  |[] -> [cntry]::colours
  | x::xs -> if canBeExtBy x cntry cht then ([cntry]@x)::xs else ([x])@(extColouring cht xs cntry)
;;

(* Question 1.4 *)

let rec removeDuplicates lst =
  match lst with
  |[] -> []
  |h::xs->h::(removeDuplicates(List.filter(fun x -> x<>h)xs))
;;

(* Question 1.5 *)

let countriesInChart (cht: chart) =
  let rec helper ch =
    match ch with
    |[]-> []
    |(c1, c2)::xs -> [c1]@[c2]@(helper xs) in removeDuplicates(helper cht)
;;

(* Quesiton 1.6 *)

let colourTheCountries (cht : chart) =
  List.fold_left(extColouring cht) [] (countriesInChart cht)
;;
njnjjnjnjnjnjnjnjjjjjjjjjjjjnjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjnjjjnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnjmmmmjjjjjjjjjjjjjjjjjjjjnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnjmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmjjjjjnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnjjjjjjjjjjjmmmml.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.[jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjnjl.nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnjjjjjjnnnnnjjjnjnjnnnnnnn
(* Question 2 *)



let rec insert comp (item: int) (list: rlist) =
  match !list with
  | None -> list := Some {data=item; next= ref None} 
  | Some c when (!(c.next)=None) -> if (comp(item, c.data)) then list:= Some {data=item; next =  cell2rlist(c)}
      else c.next := Some {data=item;next= ref None} 
  | Some {data=d; next=l } -> if (comp(item, d)) then( list:= Some {data=item;next= cell2rlist( {data=d; next=l} )} ) else (insert comp item l)