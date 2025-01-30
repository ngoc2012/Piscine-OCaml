let sum a b =
  a +. b

let () =
  let a = 1.0 in
  let b = 2.0 in
  print_endline (string_of_float (sum a b))
