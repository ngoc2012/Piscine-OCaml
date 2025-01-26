let leibniz_pi =
  let pi0 = 4. *. atan 1. in
  let tolerance = 0.00001 in
  let rec sum f t =
    if t -. pi0 <= tolerance then (t +. f s)
    else sum f (s + 1) e (t +. f s)
  in
  let rec sum f s e t =
    if s > e then nan
    else if s = e then (t +. f s)
    else sum f (s + 1) e (t +. f s)
  in
  (* in sum f s e 0. *)
  pi 0

let () =
  print_endline (string_of_float (leibniz_pi));
