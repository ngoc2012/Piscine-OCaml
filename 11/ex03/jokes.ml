let () =
  (* Ensure that the program receives exactly one command-line argument *)
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <jokes_file>\n" Sys.argv.(0);
    exit 1
  );
  let filename = Sys.argv.(1) in
  try
    let read_file filename =
      let ic = open_in filename in
      let lines = ref [] in
      try
        while true do
          let line = input_line ic in
          lines := line :: !lines
        done;
        [] (* this line will never be reached, as the loop exits on EOF *)
      with End_of_file -> 
        close_in ic;
        List.rev !lines
    in
    let jokes = read_file filename in
    if List.length jokes = 0 then
      Printf.eprintf "No jokes found in the file.\n"
    else
      Random.self_init ();
      print_endline (List.nth jokes (Random.int (List.length jokes)))
  with
  | Sys_error err -> Printf.eprintf "Error: %s\n" err
  | _ -> Printf.eprintf "An unexpected error occurred.\n"

