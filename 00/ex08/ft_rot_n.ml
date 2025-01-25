let ft_rot_n (n: int) (s: string): string =
  let rot (c: char): char =
    let nc = int_of_char c in
    let na = int_of_char 'a' in
    let nz = int_of_char 'z' in
    let nA = int_of_char 'A' in
    let nZ = int_of_char 'Z' in
    if (nc >= na) && (nc <= nz) then
      char_of_int ((nc - na + n) mod nz)
    else if (nc >= nA) && (nc <= nZ) then
      char_of_int ((nc - nA + n) mod nZ)
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
