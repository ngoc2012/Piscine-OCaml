let sequence n = match n with
  | n when n <= 0 -> []
  | _ ->
    let print_tuple (c, n) =
      print_int c;
      print_int n
    in
    let print list_strings =
      | [] -> ()
      | [t] -> print_tuple t
      | h :: t ->
        print_tuple h;
        print t
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
    let rec loop n last = match n with
      | 1 -> "1"
      | _ ->
        loop (n - 1) (string_of_int n ^ last)
    in
    print (loop n "")

let () =
