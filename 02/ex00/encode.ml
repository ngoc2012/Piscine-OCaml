let encode l =
  let rec aux l acc = match l with
      | [] -> acc
      | h :: t ->
                    let count = List.length (List.filter ((=) h) t) in
                    aux t ((h, count) :: acc)
  in aux l []
