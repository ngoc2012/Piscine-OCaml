let ft_print_comb () =
  let i = 0 in
  let j = 1 in
  let k = 2 in
  let rec comb (i: int) (j: int) (k: int) = (
    if (i < 9 && j < 9 && k < 9) then (
      if (i != j && j != k && i != k) then (
        print_int i;
        print_int j;
        print_int k
        print_string ", "
      ) else (
        if (k < 9) then (
          comb i j (k + 1)
        ) else if (j < 9) then (
          comb i (j + 1) 2
        ) else if (i < 9) then (
          comb (i + 1) 1 2
        )
        ) else (
          comb i (j + 1) 2
        )
        if 
      )
    )
  ) in
  print i j k

let () = ft_print_comb ()
