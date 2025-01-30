let () =

  let card1 = Card.newCard Card.Value.T7 Card.Color.Heart in
  let card2 = Card.newCard Card.Value.T10 Card.Color.Spade in
  let card3 = Card.newCard Card.Value.Jack Card.Color.Diamond in

  Printf.printf "Card 1: %s\n" (Card.toString card1); (* 7H *)
  Printf.printf "Card 2: %s\n" (Card.toString card2); (* 10S *)
  Printf.printf "Card 3: %s\n" (Card.toStringVerbose card3); (* Card(Jack, Diamond) *)

  (* Compare *)
  Printf.printf "Comparison between card1 and card2: %d\n" (Card.compare card1 card2); (* negative *)
  Printf.printf "Comparison between card2 and card3: %d\n" (Card.compare card2 card3); (* negative *)

  (* Max and Min *)
  let max_card = Card.max card1 card2 in
  let min_card = Card.min card2 card3 in
  Printf.printf "Max card: %s\n" (Card.toString max_card); (* 10S *)
  Printf.printf "Min card: %s\n" (Card.toString min_card); (* 10S *)

  (* Best *)
  let cards = [card1; card2; card3] in
  let best_card = Card.best cards in
  Printf.printf "Best card: %s\n" (Card.toString best_card); (* 10S *)

  (* Color checks *)
  Printf.printf "Is card1 a Heart? %b\n" (Card.isHeart card1); (* true *)
  Printf.printf "Is card2 a Spade? %b\n" (Card.isSpade card2); (* true *)
  Printf.printf "Is card3 a Diamond? %b\n" (Card.isDiamond card3); (* true *)
  Printf.printf "Is card1 a Club? %b\n" (Card.isClub card1);; (* false *)
