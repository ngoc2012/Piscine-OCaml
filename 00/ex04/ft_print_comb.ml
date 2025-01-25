let ft_print_comb () =
  let rec comb (i: int) (j: int) (k: int) = (
    if (i < j && j < k) then (
        print_int i;
        print_int j;
        print_int k;
        print_string ", "
    );
    if (k < 9) then comb i j (k + 1);
    if (j < 9) then comb i (j + 1) (j + 2);
    if (i < 9) then comb (i + 1) (i + 2) (i + 3)
  ) in
  comb 0 1 2

let () = ft_print_comb ()
