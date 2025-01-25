let ft_print_comb2 () =
  let print (i: int) (j: int) =
    if i < 10 then
      print_char '0';
    print_int i;
    print_char ' ';
    if j < 10 then
      print_char '0';
    print_int j;
  in
  let rec comb (i: int) (j: int) =
    print i j;
    if i = 98 then print_char '\n'
    else (
      print_char ',';
      print_char ' ';
      if j < 99 then
        comb i (j + 1)
      else if i < 98 then
        comb (i + 1) (i + 2)
    )
  in comb 0 1

let () = ft_print_comb2 ()
