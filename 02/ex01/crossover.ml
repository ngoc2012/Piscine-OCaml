let crossover a b =
  let rec in_list x l = match l with
    | [] -> false
    | h::t -> if h = x then true else in_list x t
  in
  let rec iter li lo = match li with
    | [] -> lo
    | h::t ->
      if in_list h b then iter t (h::lo) else iter t lo
  in
  let rec rev li lo = match li with
    | [] -> lo
    | h :: t -> rev t (h :: lo)
  in
  rev(iter a []) []

let () =
  (* Test 1: Both lists non-empty *)
  let a1 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  let b1 = [2; 4; 6; 8; 10] in
  let c1 = crossover a1 b1 in
  print_string "Test 1: "; print_int_list c1; print_newline ();

  (* Test 2: One empty list *)
  let a2 = [] in
  let b2 = [1; 2; 3] in
  let c2 = crossover a2 b2 in
  print_string "Test 2: "; print_int_list c2; print_newline ();

  (* Test 3: Both lists empty *)
  let a3 = [] in
  let b3 = [] in
  let c3 = crossover a3 b3 in
  print_string "Test 3: "; print_int_list c3; print_newline ();

  (* Test 4: First list empty, second list non-empty *)
  let a4 = [1; 2; 3; 4] in
  let b4 = [] in
  let c4 = crossover a4 b4 in
  print_string "Test 4: "; print_int_list c4; print_newline ()
