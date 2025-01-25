let fibonacci n =
  if n = 0 then 0
  else if n < 0 then (-1)
  else if n = 1 || n = 2 then 1
  else (
    let rec fibo x y s e =
      if s = e then y
      else fibo y (x + y) (s + 1) n 
    in
    fibo 1 2 3 n
  )

let () =
  print_int (fibonacci (-42));
  print_char '\n';
  print_int (fibonacci 1);
  print_char '\n';
  print_int (fibonacci 3);
  print_char '\n';
  print_int (fibonacci 6);
  print_char '\n';
  print_int (fibonacci 15);
  print_char '\n';
