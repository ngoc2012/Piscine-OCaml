let ft_print_alphabet () =
  let rec print s e =
    if s <= e then (
        print_char (char_of_int s);
        print (s + 1) e
    )
  in
  print (int_of_char 'a') (int_of_char 'z');
  print_char '\n'

let () = ft_print_alphabet ()