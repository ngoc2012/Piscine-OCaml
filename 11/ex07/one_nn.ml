type radar = float array * string

let eu_dist a b =
  let sum = ref 0.0 in
  for i = 0 to Array.length a - 1 do
    let d = a.(i) -. b.(i) in
    sum := !sum +. d *. d
  done;
  sqrt !sum

let examples_of_file filename =
  let parse_input input =
    let parts = String.split_on_char ',' input in
    let numbers = 
      let nums = List.rev (List.tl (List.rev parts)) in
      List.map float_of_string nums |> Array.of_list in
    (numbers, List.nth parts (List.length parts - 1))
  in
  try
    let file = open_in filename in
    let rec read_lines acc =
      try
      	let line = input_line file in
          if String.length line > 0 then
            try
      	    read_lines ((parse_input line) :: acc)
            with Failure _ ->
              print_endline ("An unexpected error occurred at line:\n" ^ line);
              read_lines acc
          else
            read_lines acc
      with
        | End_of_file ->
          close_in file;
          List.rev acc
    in
    read_lines []
  with
    | Sys_error err ->
      Printf.eprintf "Error: %s\n" err;
      []

let nearest_neighbor (train_set: radar list) (query: radar) : string =
  let (query_vec, _) = query in
  let closest = ref (List.hd train_set) in
  let min_dist = ref (eu_dist query_vec (fst (List.hd train_set))) in
  
  List.iter (fun (vec, label) ->
    let dist = eu_dist query_vec vec in
    if dist < !min_dist then (
      min_dist := dist;
      closest := (vec, label)
    )
  ) train_set;
  
  snd !closest
  
let () =
  let train_data = examples_of_file "../ionosphere.train.csv" in
  let test_data = examples_of_file "../ionosphere.test.csv" in
  
  List.iter (fun (vec, true_label) ->
    let predicted_label = nearest_neighbor train_data (vec, "") in
    Printf.printf "Predicted: %s, Actual: %s\n" predicted_label true_label
  ) test_data
