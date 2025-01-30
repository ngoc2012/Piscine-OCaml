type coor = {x : float; y : float}

let eu_dist a b =
  let sum_of_squares = ref 0.0 in
  for i = 0 to Array.length a - 1 do
    let dx = a.(i).x -. b.(i).x in
    let dy = a.(i).y -. b.(i).y in
    sum_of_squares := !sum_of_squares +. (dx *. dx +. dy *. dy)
  done;
  sqrt !sum_of_squares

let () =
  let a = [| {x = 1.0; y = 2.0}; {x = 3.0; y = 4.0} |] in
  let b = [| {x = 4.0; y = 6.0}; {x = 1.0; y = 8.0} |] in
  print_endline (string_of_float (eu_dist a b))
