let encode l =
  let rec aux l acc count = match l with
    | [] -> acc
    | h :: t ->
      let count = List.length (List.filter ((=) h) t) in
      aux t ((h, count) :: acc)
  in aux l [] 0

let () =
  let string_of_list lst =
    let string_of_tuple (c, n) =
      Printf.sprintf "(%c, %d)" c n
    in
    "[" ^ (String.concat "; " (List.map string_of_tuple lst)) ^ "]"
  in

  let l = ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e'] in
  print_endline (string_of_list (encode l))
