let ft_sum f s e =
  let rec sum f s e t =
    if s > e then nan
    else if s = e then (t +. f s)
    else sum f (s + 1) e (t +. f s)
  in sum f s e 0.

let () =
  print_endline (string_of_float (ft_sum (fun x -> float_of_int x) 1 5));
  print_endline (string_of_float (ft_sum (fun i -> float_of_int (i * i)) 1 10));
  print_endline (string_of_float (ft_sum (fun i -> float_of_int (i * i)) 100 10));
