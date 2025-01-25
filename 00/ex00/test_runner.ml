let () =
  let results = [
    (Ft_test_sign.ft_test_sign 5, "positive");
    (Ft_test_sign.ft_test_sign (-3), "negative");
    (Ft_test_sign.ft_test_sign 0, "positive")
  ] in
  List.iter (fun (output, expected) ->
    if output = expected then
      Printf.printf "Test passed: %s\n" output
    else
      Printf.printf "Test failed: got %s, expected %s\n" output expected
  ) results
