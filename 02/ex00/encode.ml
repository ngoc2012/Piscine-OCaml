let encode l =
  let rec rev li lo = match li with
    | [] -> lo
    | h :: t -> rev t (h :: lo)
  in
  let rec aux l acc count = match l with
    | [] -> acc
    | [t] -> (t, count + 1) :: acc
    | h :: s :: t ->
      if h = s then
        aux (s :: t) acc (count + 1)
      else
        aux (s :: t) ((h, count + 1) :: acc) 0
  in
  rev (aux l [] 0) []

let () =
  let string_of_list lst =
    let string_of_tuple (c, n) =
      Printf.sprintf "(%c, %d)" c n
    in
    "[" ^ (String.concat "; " (List.map string_of_tuple lst)) ^ "]"
  in

  let l = ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e'] in
  print_endline (string_of_list (encode l));

  let empty_list = [] in
  print_endline (string_of_list (encode empty_list))
