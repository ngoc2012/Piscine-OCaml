let leibniz_pi =
  (* let pi0 = 4. *. atan 1. in *)
  (* let tolerance = 0.00001 in *)
  (*  if t -. pi0 <= tolerance then (t +. f s) *)
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
    if s > e then nan
    else if s = e then (t +. f s)
    else sum f (s + 1) e (t +. f s)
  in
  let fi i = float_of_int (ft_power (-1) i) in
  sum fi 1 4 0. 

let () =
  print_endline (string_of_float (leibniz_pi));
