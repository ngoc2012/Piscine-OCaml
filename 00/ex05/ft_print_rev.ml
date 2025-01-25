let ft_print_rev s =
  let rec rev s i =
    if i >= 0 then (
      print_char (String.get s i);
      rev s (i - 1)
    )
  in rev s (String.length s - 1);
  print_char '\n'

let () =
  ft_print_rev "Hello world !";
  ft_print_rev "";
