let gray n =
  if n <= 0 then []
  let rec aux n acc =
    if n = 1 then acc
    else aux (n - 1) (n :: acc)
  in aux n ["0"; "1"]
let () =

