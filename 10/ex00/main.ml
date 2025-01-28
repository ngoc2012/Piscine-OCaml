let () =
  print_endline (Color.toString Spade);
  print_endline (Color.toStringVerbose Heart);
  print_endline (Color.toString Diamond);
  print_endline (Color.toString Club);
  let rec print c = match c with
    | [] -> ()
    | h::t -> print_endline (Color.toStringVerbose h); print t
  in print Color.all;;
