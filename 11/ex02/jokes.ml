let () =
  let jokes = [|
    "Why don't skeletons fight each other? They don't have the guts."; 
    "I told my computer I needed a break... now it won’t stop sending me Kit-Kats."; 
    "Why did the scarecrow win an award? Because he was outstanding in his field."; 
    "I tried to catch some fog yesterday... I mist."; 
    "I told my wife she was drawing her eyebrows too high... she looked surprised."; 
    "Why don't oysters donate to charity? Because they are shellfish."; 
    "I used to play piano by ear, but now I use my hands."; 
    "What’s orange and sounds like a parrot? A carrot."; 
    "I’m reading a book on anti-gravity. It’s impossible to put down."; 
    "Parallel lines have so much in common. It’s a shame they’ll never meet."; 
    "Did you hear about the mathematician who’s afraid of negative numbers? He’ll stop at nothing to avoid them."; 
    "I’m on a seafood diet. I see food and I eat it."; 
    "Why don’t some couples go to the gym? Because some relationships don’t work out."; 
    "I told my wife she was drawing her eyebrows too high. She looked surprised.";
  |] in
  Random.self_init ();
  let joke = jokes.(Random.int (Array.length jokes)) in
  print_endline joke

