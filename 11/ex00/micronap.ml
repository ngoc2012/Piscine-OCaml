let () =
  if Array.length Sys.argv != 2 then (
    print_endline "Usage: ./micronap seconds";
    exit 1
  ) else
    try
      let seconds = int_of_string Sys.argv.(1) in
      if seconds < 0 then exit 1;
      Unix.sleep seconds;
    with Failure _ ->
      print_endline "Seconds must be an integer";
      exit 1
