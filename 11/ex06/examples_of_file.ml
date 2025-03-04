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

let () =
  let data = examples_of_file "../ionosphere.train.csv" in
  List.iter (fun (arr, lbl) ->
    Printf.printf "Array: [|";
    Array.iter (fun x -> Printf.printf "%.5f; " x) arr;
    Printf.printf "|], Label: %s\n" lbl
  ) data;
  print_endline ("Number of examples: " ^ (string_of_int (List.length data)))
