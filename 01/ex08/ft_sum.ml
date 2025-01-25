let rec ft_sum f s e =
  if s > e then nan
  else if s = e then f s
  else f s +. ft_sum f (s + 1) e

let () =
  print_endline (string_of_float (ft_sum (fun x -> float_of_int x) 1 5));
  print_endline (string_of_float (ft_sum (fun i -> float_of_int (i * i)) 1 10));
  print_endline (string_of_float (ft_sum (fun i -> float_of_int (i * i)) 100 10));
