let sequence n = match n with
  | n when n <= 0 -> print_endline ""
  | _ ->
    let rec print list_int = match list_int with
      | [] -> ()
      | [t] -> print_int t
      | h :: t ->
        print_int h;
        print t
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
    print (loop n [1]);
    print_endline ""

let () =
  sequence 1;;
  sequence 2;;
  sequence 3;;
  sequence 4;;
  sequence 5;;
  sequence 6;;
  sequence 7;;
  sequence 8;;
  sequence 9;;
