let ft_print_alphabet () =
  let start = 97 in
  let end_ = 122 in
  let rec print (start: int) (end_: int) =
    if start <= end_ then (
        print_char (char_of_int start);
        print (start + 1) end_
    )
  in
  print start end_;
  print_char '\n'

let () = ft_print_alphabet ()