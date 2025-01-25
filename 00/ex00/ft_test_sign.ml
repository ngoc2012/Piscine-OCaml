let ft_test_sign (n: int) =
  if n < 0 then print_endline "negative" else print_endline "positive"

let () =
  let test_values = [42; 0; -42] in
  List.iter (fun n ->
    Printf.printf "Test with [%d]: " n;
    ft_test_sign n
  ) test_values