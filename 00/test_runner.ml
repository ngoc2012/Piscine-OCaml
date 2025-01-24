#use "ft_test_sign.ml";;

let () =
  let results = [
    (test_sign 5, "positive");
    (test_sign (-3), "negative");
    (test_sign 0, "zero")
  ] in
  List.iter (fun (output, expected) ->
    if output = expected then
      Printf.printf "Test passed: %s\n" output
    else
      Printf.printf "Test failed: got %s, expected %s\n" output expected
  ) results