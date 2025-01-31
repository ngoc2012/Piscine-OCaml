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

let k_nearest_neighbors (train_set: radar list) (k: int) (query: radar) : string =
  let (query_vec, _) = query in
  (* Compute the distances between the query point and each point in the train set *)
  let distances = List.map (fun (vec, label) -> 
    (eu_dist query_vec vec, label)
  ) train_set in

  (* Sort the distances in ascending order *)
  let sorted_distances = List.sort (fun (d1, _) (d2, _) -> compare d1 d2) distances in
  
  (* Get the first K elements from the sorted list *)
  let nearest_k = List.sub sorted_distances 0 (min k (List.length sorted_distances)) in
  
  (* Count the occurrences of each label in the nearest K neighbors *)
  let label_count = 
    List.fold_left (fun acc (_, label) ->
      let count = try List.assoc label acc with Not_found -> 0 in
      (label, count + 1) :: List.remove_assoc label acc
    ) [] nearest_k
  in

  (* Find the label with the maximum count *)
  let most_common_label = 
    List.fold_left (fun acc (label, count) ->
      match acc with
      | None -> Some (label, count)
      | Some (_, max_count) when count > max_count -> Some (label, count)
      | _ -> acc
    ) None label_count 
  in

  (* Return the most common label *)
  match most_common_label with
  | Some (label, _) -> label
  | None -> failwith "No neighbors found"

let accuracy f (test_set: radar list) (train_set: radar list) : float =
  let correct_predictions = 
    List.fold_left (fun acc (vec, true_label) ->
      let predicted_label = f train_set (vec, "") in
      if predicted_label = true_label then acc + 1 else acc
    ) 0 test_set
  in
  float_of_int correct_predictions /. float_of_int (List.length test_set)
  
let () =
  let train_data = examples_of_file "../ionosphere.train.csv" in
  let test_data = examples_of_file "../ionosphere.test.csv" in
  
  List.iter (fun (vec, true_label) ->
    let predicted_label = nearest_neighbor train_data (vec, "") in
    Printf.printf "Predicted: %s, Actual: %s\n" predicted_label true_label
  ) test_data

  let acc = accuracy nearest_neighbor test_data train_data in
  Printf.printf "Accuracy: %.2f%%\n" (acc *. 100.0)
