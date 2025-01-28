let () =
  (* Test toInt function *)
  let () =
    assert (toInt T2 = 1);
    assert (toInt T3 = 2);
    assert (toInt T10 = 9);
    assert (toInt Jack = 10);
    assert (toInt Queen = 11);
    assert (toInt King = 12);
    assert (toInt As = 13);
    print_endline "toInt tests passed"
  in

  (* Test toString function *)
  let () =
    assert (toString T2 = "2");
    assert (toString T3 = "3");
    assert (toString T10 = "10");
    assert (toString Jack = "J");
    assert (toString Queen = "Q");
    assert (toString King = "K");
    assert (toString As = "A");
    print_endline "toString tests passed"
  in

  (* Test toStringVerbose function *)
  let () =
    assert (toStringVerbose T2 = "2");
    assert (toStringVerbose T3 = "3");
    assert (toStringVerbose T10 = "10");
    assert (toStringVerbose Jack = "Jack");
    assert (toStringVerbose Queen = "Queen");
    assert (toStringVerbose King = "King");
    assert (toStringVerbose As = "As");
    print_endline "toStringVerbose tests passed"
  in

  (* Test next function *)
  let () =
    assert (next T2 = T3);
    assert (next T3 = T4);
    assert (next T10 = Jack);
    assert (next Queen = King);
    assert (next King = As);
    (try let _ = next As in assert false with Invalid_argument _ -> ());
    print_endline "next tests passed"
  in

  (* Test previous function *)
  let () =
    assert (previous T3 = T2);
    assert (previous T4 = T3);
    assert (previous Jack = T10);
    assert (previous King = Queen);
    assert (previous As = King);
    (try let _ = previous T2 in assert false with Invalid_argument _ -> ());
    print_endline "previous tests passed"
  in

  print_endline "All tests passed successfully";;


