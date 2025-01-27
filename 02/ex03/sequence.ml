let sequence n = match n with
  | n when n <= 0 -> []
  | _ ->
    let print list_int =
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
    let rec loop n last = match n with
      | 1 -> [1]
      | _ ->
        loop (n - 1) (aux last [] 0)
    in
    print (loop n [])

let () =
  sequence 1;;
