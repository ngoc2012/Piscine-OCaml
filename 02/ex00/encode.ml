let encode l =
  let rec aux l acc count = match l with
    | [] -> acc
    | t :: -> (t, count + 1) :: acc
    | h :: t ->
      if h = t then
        aux t acc (count + 1)
      else
        aux t ((h, count) :: acc) 0
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
