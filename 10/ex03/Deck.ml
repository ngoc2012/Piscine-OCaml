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
  
  let next x =
    let rec check a = match a with
      | [] -> invalid_arg "Value.next"
      | [h] -> invalid_arg "Value.next"
      | h :: s :: t ->
        if h = x then s else check (s :: t)
    in check all    
  
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

type t = Card.t list

let swap lst i j =
  let len = List.length lst in
  if i = j || i < 0 || j < 0 || i >= len || j >= len then
    lst
  else
    let rec aux t k acc = match t with
      | [] -> List.rev acc
      | h :: t when k = i -> aux t (k + 1) (List.nth lst j :: acc)
      | h :: t when k = j -> aux t (k + 1) (List.nth lst i :: acc)
      | h :: t -> aux t (k + 1) (h :: acc)
    in aux lst 0 []
  
let shuffle lst =
  Random.self_init ();
  let rec loop l n = match n with
    | n when n <= 1 -> l
    | _ -> loop (swap l (Random.int n) n) (n - 1)
  in loop lst ((List.length lst) - 1)

let newDeck () = Card.all

let toStringList deck =
  List.map Card.toString deck

let toStringListVerbose deck =
  List.map Card.toStringVerbose deck

let drawCard deck =
  match deck with
  | [] -> failwith "Deck is empty: Cannot draw a card."
  | card :: rest -> (card, rest)
