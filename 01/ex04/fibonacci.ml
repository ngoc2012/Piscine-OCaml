let rec fibonacci n =
  if n = 0 then 0
  else if n < 0 then (-1)
  else if n = 1 then 1
  else fibonacci (n - 1) + fibonacci (n - 2)

let () =
  print_int (fibonacci (-42));
  print_int (fibonacci 1);
  print_int (fibonacci 3);
  print_int (fibonacci 6);
