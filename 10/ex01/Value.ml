type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

(** Interger representation of a card value, from 1 for T2 to 13 for As *)
let toInt = function
    | T2    -> 1
    | T3    -> 2
    | T4    -> 3
    | T5    -> 4
    | T6    -> 5
    | T7    -> 6
    | T8    -> 7
    | T9    -> 8
    | T10   -> 9
    | Jack  -> 10
    | Queen -> 11
    | King  -> 12
    | As    -> 13

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