(* Helper function to capture printed output *)
let capture_output f x =
  let stdout_copy = Unix.dup Unix.stdout in
  let temp_fd = Unix.openfile "temp_output" [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
  Unix.dup2 temp_fd Unix.stdout;  (* Redirect stdout to temp_fd *)
  close_out (Unix.out_channel_of_descr temp_fd);

  f x;  (* Call the function that prints *)

  Unix.dup2 stdout_copy Unix.stdout;  (* Restore stdout *)
  Unix.close stdout_copy;

  let in_channel = open_in "temp_output" in
  let result = input_line in_channel in
  close_in in_channel;
  Sys.remove "temp_output";  (* Clean up *)
  result

(* Test ft_test_sign with captured output *)
let () =
  let results = [
    (capture_output Ft_test_sign.ft_test_sign 5, "positive");
    (capture_output Ft_test_sign.ft_test_sign (-3), "negative");
    (capture_output Ft_test_sign.ft_test_sign 0, "positive")  (* Adjust expected output if needed *)
  ] in
  List.iter (fun (output, expected) ->
    if output = expected then
      Printf.printf "Test passed: got %s\n" output
    else
      Printf.printf "Test failed: got %s, expected %s\n" output expected
  ) results
