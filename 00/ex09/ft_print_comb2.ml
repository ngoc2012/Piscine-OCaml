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
    if i < 10 then
      if j < 10 then

let () = ft_print_comb2 ()
