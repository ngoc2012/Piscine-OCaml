let ft_is_palindrome s =
  let rec is_palindrome s i j =
    if i >= j then true
    else if (String.get s i) <> (String.get s j) then false
    else is_palindrome s (i + 1) (j - 1)
  in is_palindrome s 0 (String.length s - 1)

let () =
  let result = ft_is_palindrome "radar" in
  print_endline (string_of_bool result);
  let result = ft_is_palindrome "madam" in
  print_endline (string_of_bool result);
  let result = ft_is_palindrome "car" in
  print_endline (string_of_bool result);
  let result = ft_is_palindrome "" in
  print_endline (string_of_bool result);
