let () =
  let jokes = [|
    "Why don't skeletons fight each other? They don't have the guts."; 
    "I told my computer I needed a break... now it won’t stop sending me Kit-Kats."; 
    "Why did the scarecrow win an award? Because he was outstanding in his field."; 
    "I tried to catch some fog yesterday... I mist."; 
    "I told my wife she was drawing her eyebrows too high... she looked surprised."; 
  |] in
  Random.self_init ();
  let joke = jokes.(Random.int (Array.length jokes)) in
  print_endline joke

