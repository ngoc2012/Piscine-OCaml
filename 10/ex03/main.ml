let () =
  (* Create a new deck *)
  let deck = Deck.newDeck () in

  (* Display the deck in different formats *)
  Printf.printf "Deck (string representation):\n%s\n"
    (String.concat " " (Deck.toStringList deck));

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