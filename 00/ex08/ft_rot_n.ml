let ft_rot_n (n: int) (s: string): string =
  let rot (c: char): char =
    let nc = int_of_char c
    and na = int_of_char 'a'
    and nA = int_of_char 'A' in
    if (nc >= na) && (nc <= (int_of_char 'z')) then
      char_of_int (((nc - na + n) mod 26) + na)
    else if (nc >= nA) && (nc <= (int_of_char 'Z')) then
      char_of_int (((nc - nA + n) mod 26) + nA)
    else c
  in String.map rot s

let () =
  let res = ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz" in
  print_endline res;
  let res = ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz" in
  print_endline res;
  let res = ft_rot_n 42 "0123456789" in
  print_endline res;
  let res = ft_rot_n 2 "OI2EAS67B9" in
  print_endline res;
  let res = ft_rot_n 0 "Damned !" in
  print_endline res;
  let res = ft_rot_n 42 "" in
  print_endline res;
  let res = ft_rot_n 1 "NBzlk qnbjr !" in
  print_endline res
