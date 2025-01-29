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

module Deck =
struct
  module Card = Card

  (* A deck is represented as a list of cards. *)
  type t = Card.t list

  (** Shuffle a list using the Fisher-Yates algorithm *)
  let swap lst i j =
    let rec aux index acc lst = match lst with
      | [] -> List.rev acc
      | h :: t when index = i -> aux (index + 1) (List.nth lst j :: acc) t
      | h :: t when index = j -> aux (index + 1) (List.nth lst i :: acc) t
      | h :: t -> aux (index + 1) (h :: acc) t
    in
    if i = j || i < 0 || j < 0 || i >= List.length lst || j >= List.length lst then
      lst
    else
      aux 0 [] lst
    
  let shuffle lst =
    let rec loop l n = match n with
      | n when n <= 1 -> l
      | _ ->
        loop l (swap lst (Random.int n) n) (n - 1)
    in loop lst ((List.length lst) - 1)

  (** Creates a new deck of 52 cards in random order. *)
  let newDeck () =
    Random.self_init (); (* Initialize random seed *)
    shuffle Card.all

  (** Returns a list of the string representations of each card in the deck. *)
  let toStringList deck =
    List.map Card.toString deck

  (** Returns a list of the verbose string representations of each card in the deck. *)
  let toStringListVerbose deck =
    List.map Card.toStringVerbose deck

  (** Draws the first card from the deck. *)
  let drawCard deck =
    match deck with
    | [] -> failwith "Deck is empty: Cannot draw a card."
    | card :: rest -> (card, rest)
end


let () =
  (* Create a new deck *)
  let deck = Deck.newDeck () in

  (* Display the deck in different formats *)
  Printf.printf "Deck (string representation):\n%s\n"
    (String.concat " " (Deck.toStringList deck));

  Printf.printf "Deck (verbose representation):\n%s\n"
    (String.concat ", " (Deck.toStringListVerbose deck));

  (* Draw a card from the deck *)
  let card, remaining_deck = Deck.drawCard deck in
  Printf.printf "Drew card: %s\n" (Deck.Card.toString card);
  Printf.printf "Remaining deck size: %d\n" (List.length remaining_deck);

  (* Draw all cards to demonstrate exception *)
  let rec draw_all deck =
    try
      let card, rest = Deck.drawCard deck in
      Printf.printf "Drew card: %s\n" (Deck.Card.toString card);
      draw_all rest
    with Failure msg ->
      Printf.printf "Exception: %s\n" msg
  in
  draw_all remaining_deck
