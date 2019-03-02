let mysqrt (x:float) =
  let rec nrsqrt((g: float), (x: float))=  

    if close(square(g), x) then g  

    else nrsqrt(((g +.(x /. g))/. 2.0), x)  in nrsqrt(1., x)



let cube_root (x:float) = 
  let rec nrcbrt((g: float), (x: float))= 
    if close(cube(g), x) then g 

    else nrcbrt(( ((2.0 *. g)+.( x /. square(g)))/.(3.0)), x) in nrcbrt(1., x)

let fast_exp (base, power) = 

  let rec helper(base, power, acc) = 

    if base = 0 then 0 

    else if power = 0 then 1 

    else if power = 1 then acc*base 

    else if odd(power) then   helper(base, (power-1), acc*base)  

    else   helper(base*base, (power/2), acc) in helper(base, power, 1)  
                                         

                           

