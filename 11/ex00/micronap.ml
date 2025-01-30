let () =
  if Array.length Sys.argv != 2 then (
    print_endline "Usage: ./micronap seconds";
    exit 1
  ) else
    let seconds = try int_of_string Sys.argv.(1) with Failure _ ->
      print_endline "Seconds must be an integer"; 0 in
    if seconds < 0 then (
       print_endline "Seconds must be a positive integer";
       exit 1
    );
    if seconds > 0 then Unix.sleep seconds;
    exit 0
