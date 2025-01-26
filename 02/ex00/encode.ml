let encode l =
  let rec aux l acc = match l with
    | [] -> acc
    | h :: t ->
      let count = List.length (List.filter ((=) h) t) in
      aux t ((h, count) :: acc)
  in aux l []

let () =
  let l = ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e'] in
  print_endline (string_of_list (encode l))
