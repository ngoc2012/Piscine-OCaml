let leibniz_pi delta =
  if delta <= 0. then -1
  else (
    let pi0 = 4. *. atan 1. in
    let ft_power n p =
      if n = 0 then 0
      else if p = 0 then 1
      else (
        let rec pow n p np =
          if p = 0 then np
          else pow n (p - 1) (n * np)
        in pow n p 1
      )
    in
    let abs x = if x >= 0. then x else -.x in
    let rec sum f s e t =
      let res = t +. f s in
      if s > e then -1
      else if (s = e) || (abs (res -. pi0) <= delta) then s
      else sum f (s + 1) e res
    in
    let fi i = 4. *. (float_of_int (ft_power (-1) i)) /. (2. *. float_of_int(i) +. 1.) in
    sum fi 1 100000 4. 
  )

let () =
  let ft_power n p =
    if n = 0 then 0
    else if p = 0 then 1
    else (
      let rec pow n p np =
        if p = 0 then np
        else pow n (p - 1) (n * np)
      in pow n p 1
    )
  in
  let rec sum f s e t =
    let res = t +. f s in
    if s > e then nan
    else if s = e then res
    else sum f (s + 1) e res
  in
  let fi i = 4. *. (float_of_int (ft_power (-1) i)) /. (2. *. float_of_int(i) +. 1.) in
  let pi0 = 4. *. atan 1. in
  print_endline (string_of_int (leibniz_pi 0.0001));
  print_endline (string_of_int (leibniz_pi 0.00004));
  let n = leibniz_pi 0.00007 in
  print_endline (string_of_int (n));
  let res = sum fi 0 n 0. in
  print_endline (string_of_float (pi0));
  print_endline (string_of_float (res));
  print_endline (string_of_float (res -. pi0));
  print_endline (string_of_float (sum fi 0 1000 0.));
