let leibniz_pi =
  let pi0 = 4. *. atan 1.
  and tolerance = 0.00001
  let rec pi i =
    if pi -. pi0 < tolerance then pi
    else 4. * 
  in
  pi 0

let () =
  print_endline (string_of_float (leibniz_pi));
