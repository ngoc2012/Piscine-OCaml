module Color = struct
  type t = Spade | Heart | Diamond | Club
  
  let all = [Spade; Heart; Diamond; Club]
  
  let toString = function
    | Spade -> "S"
    | Heart -> "H"
    | Diamond -> "D"
    | Club -> "C"
  
  let toStringVerbose = function
    | Spade -> "Spade"
    | Heart -> "Heart"
    | Diamond -> "Diamond"
    | Club -> "Club"
end


module Value = struct
  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
  
  let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]
  
  (** Interger representation of a card value, from 1 for T2 to 13 for As *)
  let toInt = function
    | T2 -> 2 | T3 -> 3 | T4 -> 4 | T5 -> 5 | T6 -> 6
    | T7 -> 7 | T8 -> 8 | T9 -> 9 | T10 -> 10
    | Jack -> 11 | Queen -> 12 | King -> 13 | As -> 14
  
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
end

module Card = struct
  type t = { value: Value.t; color: Color.t }

  let newCard value color = { value; color }

  let allSpades =
    List.map (fun v -> newCard v Color.Spade) Value.all

  let allHearts =
    List.map (fun v -> newCard v Color.Heart) Value.all

  let allDiamonds =
    List.map (fun v -> newCard v Color.Diamond) Value.all

  let allClubs =
    List.map (fun v -> newCard v Color.Club) Value.all

  let all = allSpades @ allHearts @ allDiamonds @ allClubs

  let getValue card = card.value

  let getColor card = card.color

  let toString card =
    let value_str = Value.toString card.value in
    let color_str = Color.toString card.color in
    value_str ^ color_str

  let toStringVerbose card =
    let value_str = Value.toStringVerbose card.value in
    let color_str = Color.toStringVerbose card.color in
    "Card(" ^ value_str ^ ", " ^ color_str ^ ")"

  let compare card1 card2 =
    (Value.toInt card1.value) - (Value.toInt card2.value)

  let max card1 card2 =
    if compare card1 card2 >= 0 then card1 else card2

  let min card1 card2 =
    if compare card1 card2 <= 0 then card1 else card2

  let best cards =
    let rec find c = match c with
      | [] -> invalid_arg "Card.best"	
      | [h] -> h
      | h :: s :: t -> find ((max h s) :: t)
    in find cards

  let isOf card color = card.color = color

  let isSpade card = isOf card Color.Spade

  let isHeart card = isOf card Color.Heart

  let isDiamond card = isOf card Color.Diamond

  let isClub card = isOf card Color.Club
end

let () =
  let open Card in

  let card1 = newCard Value.T7 Color.Heart in
  let card2 = newCard Value.T10 Color.Spade in
  let card3 = newCard Value.Jack Color.Diamond in

  (* toString and toStringVerbose *)
  Printf.printf "Card 1: %s\n" (toString card1); (* 7H *)
  Printf.printf "Card 2: %s\n" (toString card2); (* 10S *)
  Printf.printf "Card 3: %s\n" (toStringVerbose card3); (* Card(Jack, Diamond) *)

  (* Compare *)
  Printf.printf "Comparison between card1 and card2: %d\n" (compare card1 card2); (* negative *)
  Printf.printf "Comparison between card2 and card3: %d\n" (compare card2 card3); (* negative *)

  (* Max and Min *)
  let max_card = max card1 card2 in
  let min_card = min card2 card3 in
  Printf.printf "Max card: %s\n" (toString max_card); (* 10S *)
  Printf.printf "Min card: %s\n" (toString min_card); (* 10S *)

  (* Best *)
  let cards = [card1; card2; card3] in
  let best_card = best cards in
  Printf.printf "Best card: %s\n" (toString best_card); (* 10S *)

  (* Color checks *)
  Printf.printf "Is card1 a Heart? %b\n" (isHeart card1); (* true *)
  Printf.printf "Is card2 a Spade? %b\n" (isSpade card2); (* true *)
  Printf.printf "Is card3 a Diamond? %b\n" (isDiamond card3); (* true *)
  Printf.printf "Is card1 a Club? %b\n" (isClub card1);; (* false *)
