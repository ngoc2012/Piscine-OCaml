let eu_dist a b =
  let sum = ref 0.0 in
  for i = 0 to Array.length a - 1 do
    let d = a.(i) -. b.(i) in
    sum := !sum +. d *. d
  done;
  sqrt !sum

let () =
  let a = [|1.0; 2.0; 3.0|] in
  let b = [|4.0; 5.0; 6.0|] in
  print_endline (string_of_float (eu_dist a b));

  let c = [|1.0; 1.0; 1.0|] in
  let d = [|1.0; 1.0; 1.0|] in
  print_endline (string_of_float (eu_dist c d));;
