let examples_of_file filename =
	let file = open_in filename in
	let rec loop () =
		try
			let line = input_line file in
			print_endline line;
			loop ()
		with End_of_file -> close_in file
	in
	loop ()

let parse_input input =
  (* Split the string by commas *)
  let parts = String.split_on_char ',' input in

  (* Convert the first parts to floats and store them in an array *)
  let numbers = 
    List.sub parts 0 (List.length parts - 1)
    |> List.map float_of_string 
    |> Array.of_list
  in

  (* The last part is the string "g" *)
  let label = List.nth parts (List.length parts - 1) in

  (numbers, label)

let () =
  let input = "1.0,0.5,0.3,g" in
  let result = parse_input input in
  match result with
  | (arr, str) -> 
    Printf.printf "Array: [|";
    Array.iter (fun x -> Printf.printf "%.1f; " x) arr;
    Printf.printf "|], Label: %s\n" str