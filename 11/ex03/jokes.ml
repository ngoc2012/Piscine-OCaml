let read_jokes_from_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file -> 
      close_in ic;
      List.rev acc
  in
  read_lines []

let () =
  (* Ensure that the program receives exactly one command-line argument *)
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <jokes_file>\n" Sys.argv.(0);
    exit 1
  );
  let filename = Sys.argv.(1) in
  try
    let jokes = read_jokes_from_file filename in
    if List.length jokes = 0 then
      Printf.eprintf "No jokes found in the file.\n"
    else
      let random_joke = List.nth jokes (Random.int (List.length jokes)) in
      print_endline random_joke
  with
  | Sys_error err -> Printf.eprintf "Error: %s\n" err
  | _ -> Printf.eprintf "An unexpected error occurred.\n"

