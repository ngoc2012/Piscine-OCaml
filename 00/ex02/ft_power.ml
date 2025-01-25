let rec ft_power n p =
  if p = 0 then 1
  else if n = 0 then 0
  else n * (ft_power n (p - 1))

let () =
  print_int (ft_power 2 4);
  print_char ('\n');
  print_int (ft_power 3 0);
  print_char ('\n');
  print_int (ft_power 0 5);
  print_char ('\n');
