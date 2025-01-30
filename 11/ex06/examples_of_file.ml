let examples_of_file filename =
  let parse_input input =
    let parts = String.split_on_char ',' input in
    let numbers = 
      let nums = List.rev (List.tl (List.rev parts)) in
      List.map float_of_string nums |> Array.of_list in
    (numbers, List.nth parts (List.length parts - 1))
  in
  let file = open_in filename in
  let rec read_lines acc =
    try
    	let line = input_line file in
        if String.length line > 0 then
    	  read_lines ((parse_input line) :: acc)
        else
          read_lines acc
    with End_of_file ->
      close_in file;
      List.rev acc
  in
  read_lines []

let () =
  if (Array.length Sys.argv) < 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let data = examples_of_file Sys.argv.(1) in
    List.iter (fun (arr, lbl) ->
      Printf.printf "Array: [|";
      Array.iter (fun x -> Printf.printf "%.1f; " x) arr;
      Printf.printf "|], Label: %s\n" lbl
    ) data
