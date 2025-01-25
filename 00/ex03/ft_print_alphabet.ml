let ft_print_alphabet () =
  let rec print (start: int) (end_: int) =
    if start <= end_ then (
        print_char (char_of_int start);
        print (start + 1) end_
    )
  in
  print (int_of_char 'a') (int_of_char 'z');
  print_char '\n'

let () = ft_print_alphabet ()