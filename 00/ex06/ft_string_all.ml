let ft_string_all f s =
  let rec loop i =
    if i < 0 then true
    else if f (String.get s i) then loop (i - 1)
    else false
  in loop (String.length s - 1)

let () =
  let is_digit c = c >= '0' && c <= '9' in
  let res = ft_string_all is_digit "0123456789" in
  print_endline (string_of_bool res);
  let res = ft_string_all is_digit "012EA56789" in
  print_endline (string_of_bool res)
