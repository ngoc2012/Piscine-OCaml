let sequence n = match n with
  | n when n <= 0 -> []
  | _ ->
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
        if n = 1 then acc
        loop (n - 1) (string_of_int n ^ last)
    in loop n ""

let () =
