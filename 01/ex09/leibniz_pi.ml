let leibniz_pi =
  let pi0 = 4. *. atan 1.
  and tolerance = 0.00001 in
  if s > e then nan
  else if s = e then f s
  else f s +. ft_sum f (s + 1) e

let () =
  print_endline (string_of_float (leibniz_pi));
