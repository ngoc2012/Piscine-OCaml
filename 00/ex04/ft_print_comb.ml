let ft_print_comb () =
  let i = 0 in
  let j = 1 in
  let k = 2 in
  let print (i: int) (j: int) (k: int) = (
    print_int i;
    print_int j;
    print_int k
  )
  in
  print i j k

let () = ft_print_comb ()
