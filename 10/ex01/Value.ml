type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

(** Interger representation of a card value, from 1 for T2 to 13 for As *)
let toInt x =
  let rec check a index =
    | [] -> -1
    | h :: t ->
      if h = x then index + 1
      else check t index + 1
  in check all 0

(** returns "2", ..., "10", "J", "Q", "K" or "A" *)
val toString : t -> string
(** returns "2", ..., "10", "Jack", "Queen", "King" or "As" *)
val toStringVerbose : t -> string
(** Returns the next value, or calls invalid_arg if argument is As *)
val next : t -> t
(** Returns the previous value, or calls invalid_arg if argument is T2 *)
val previous : t -> t
