let rec repeat_string ?str:(s = "x") n =
  let repeat s n =
    if n < 0 then "Error"
    else if n = 0 then ""
    else s ^ repeat_string ~str:s (n - 1)
  in
  repeat s n

let () =
  print_endline(repeat_string (-1));
  print_endline(repeat_string 0);
  print_endline(repeat_string ~str:"Toto" 1);
  print_endline(repeat_string 2);
  print_endline(repeat_string ~str:"a" 5);
  print_endline(repeat_string ~str:"what" 3);
