let () =
  let results = [
    (ft_test_sign 5, "positive");
    (ft_test_sign (-3), "negative");
    (ft_test_sign 0, "positive")
  ] in
  List.iter (fun (output, expected) ->
    if output = expected then
      Printf.printf "Test passed: %s\n" output
    else
      Printf.printf "Test failed: got %s, expected %s\n" output expected
  ) results