let sequence n = match n with
  | n when n <= 0 -> ""
  | _ ->
    let rec print list_int s = match list_int with
      | [] -> s
      | [t] -> s ^ (string_of_int t)
      | h :: t ->
        print t (s ^ (string_of_int h))
    in
    let rec aux l acc count = match l with
      | [] -> acc
      | [t] -> (count + 1) :: t :: acc
      | h :: s :: t ->
        if h = s then
          aux (s :: t) acc (count + 1)
        else
          aux (s :: t) ((count + 1) :: h :: acc) 0
    in
    let rec rev li lo = match li with
      | [] -> lo
      | h :: t -> rev t (h :: lo)
    in
    let rec loop n last = match n with
      | 1 -> last
      | _ ->
        loop (n - 1) (aux (rev last []) [] 0)
    in
    print (loop n [1]) ""

let () =
  print_endline (sequence 1);;
  print_endline (sequence 2);;
  print_endline (sequence 3);;
  print_endline (sequence 4);;
  print_endline (sequence 5);;
  print_endline (sequence 6);;
  print_endline (sequence 7);;
  print_endline (sequence 8);;
  print_endline (sequence 9);;
  print_endline (sequence 1000);;
