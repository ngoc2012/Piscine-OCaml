let ft_string_all (f: char -> bool) (s: string): bool =
  let rec loop (i: int): bool =
    if i < 0 then true
    else if f s.[i] then loop (i - 1)
    else false
  in loop (String.length s - 1)

let () =
  let is_digit c = c >= '0' && c <= '9';
  let res = ft_string_all is_digit "0123456789";
  print_endline (string_of_bool res);
  let res = ft_string_all is_digit "012EA56789";
  print_endline (string_of_bool res)
