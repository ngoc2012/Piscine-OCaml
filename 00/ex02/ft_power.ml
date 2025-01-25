let rec ft_power n p =
  if n = 0 then 0
  else if p = 0 then 1
  else (
    let rec pow n p np =
      if p = 0 then np
      else pow n (p - 1) (n * np)
    in pow n p 1
  )

let () =
  print_int (ft_power 2 4);
  print_char ('\n');
  print_int (ft_power 3 0);
  print_char ('\n');
  print_int (ft_power 0 5);
  print_char ('\n');
  print_int (ft_power 4 12);
  print_char ('\n');
