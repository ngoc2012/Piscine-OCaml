type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

(** Interger representation of a card value, from 1 for T2 to 13 for As *)
let toInt x =
  let rec check a index = match a with
    | [] -> -1
    | h :: t ->
      if h = x then index + 1
      else check t index + 1
  in check all 0

(** returns "2", ..., "10", "J", "Q", "K" or "A" *)
let toString = function
  | T2 -> "2"
  | T3 -> "3"
  | T4 -> "4"
  | T5 -> "5"
  | T6 -> "6"
  | T7 -> "7"
  | T8 -> "8"
  | T9 -> "9"
  | T10 -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | As -> "A"

(** returns "2", ..., "10", "Jack", "Queen", "King" or "As" *)
let toStringVerbose = function
  | T2 -> "2"
  | T3 -> "3"
  | T4 -> "4"
  | T5 -> "5"
  | T6 -> "6"
  | T7 -> "7"
  | T8 -> "8"
  | T9 -> "9"
  | T10 -> "10"
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"
  | As -> "As"

(** Returns the next value, or calls invalid_arg if argument is As *)
let next x =
  let rec check a = match a with
    | [] -> invalid_arg "Value.next"
    | [h] -> invalid_arg "Value.next"
    | h :: s :: t ->
      if h = x then s else check (s :: t)
  in check all    

(** Returns the previous value, or calls invalid_arg if argument is T2 *)
let previous x =
  let rec check a = match a with
    | [] -> invalid_arg "Value.previous"
    | [h] -> invalid_arg "Value.previous"
    | h :: s :: t ->
      if h = x then invalid_arg "Value.previous"
      else if s = x then h
      else check (s :: t)
  in check all    

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


