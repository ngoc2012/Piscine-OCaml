let () =
  (* Test toInt function *)
  let () =
    print_endline ("toInt T2: " ^ string_of_int (Value.toInt T2));
    print_endline ("toInt T3: " ^ string_of_int (Value.toInt T3));
    print_endline ("toInt T10: " ^ string_of_int (Value.toInt T10));
    print_endline ("toInt Jack: " ^ string_of_int (Value.toInt Jack));
    print_endline ("toInt Queen: " ^ string_of_int (Value.toInt Queen));
    print_endline ("toInt King: " ^ string_of_int (Value.toInt King));
    print_endline ("toInt As: " ^ string_of_int (Value.toInt As));
  in

  (* Test toString function *)
  let () =
    print_endline ("toString T2: " ^ Value.toString T2);
    print_endline ("toString T3: " ^ Value.toString T3);
    print_endline ("toString T10: " ^ Value.toString T10);
    print_endline ("toString Jack: " ^ Value.toString Jack);
    print_endline ("toString Queen: " ^ Value.toString Queen);
    print_endline ("toString King: " ^ Value.toString King);
    print_endline ("toString As: " ^ Value.toString As);
  in

  (* Test toStringVerbose function *)
  let () =
    print_endline ("toStringVerbose T2: " ^ Value.toStringVerbose T2);
    print_endline ("toStringVerbose T3: " ^ Value.toStringVerbose T3);
    print_endline ("toStringVerbose T10: " ^ Value.toStringVerbose T10);
    print_endline ("toStringVerbose Jack: " ^ Value.toStringVerbose Jack);
    print_endline ("toStringVerbose Queen: " ^ Value.toStringVerbose Queen);
    print_endline ("toStringVerbose King: " ^ Value.toStringVerbose King);
    print_endline ("toStringVerbose As: " ^ Value.toStringVerbose As);
  in

  (* Test next function *)
  let () =
    print_endline ("next T2: " ^ Value.toString (Value.next T2));
    print_endline ("next T3: " ^ Value.toString (Value.next T3));
    print_endline ("next T10: " ^ Value.toString (Value.next T10));
    print_endline ("next Queen: " ^ Value.toString (Value.next Queen));
    print_endline ("next King: " ^ Value.toString (Value.next King));
    (try print_endline ("next As: " ^ Value.toString (Value.next As)) with Invalid_argument _ -> print_endline "next As: Invalid_argument");
  in

  (* Test previous function *)
  let () =
    print_endline ("previous T3: " ^ Value.toString (Value.previous T3));
    print_endline ("previous T4: " ^ Value.toString (Value.previous T4));
    print_endline ("previous Jack: " ^ Value.toString (Value.previous Jack));
    print_endline ("previous King: " ^ Value.toString (Value.previous King));
    print_endline ("previous As: " ^ Value.toString (Value.previous As));
    (try print_endline ("previous T2: " ^ Value.toString (Value.previous T2)) with Invalid_argument _ -> print_endline "previous T2: Invalid_argument");
  in

  print_endline "Tests completed."

