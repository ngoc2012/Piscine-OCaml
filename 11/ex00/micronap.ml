let my_sleep () = Unix.sleep 1

let () =
  if Array.length Sys.argv < 2 then
    exit 1
  else
    let arg = Sys.argv.(1) in
    try
      let seconds = int_of_string arg in
      if seconds < 0 then exit 1;
      for _ = 1 to seconds do
        my_sleep ()
      done
    with Failure _ -> exit 1

