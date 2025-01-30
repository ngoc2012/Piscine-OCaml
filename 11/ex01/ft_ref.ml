type 'a ft_ref = 'a -> 'a * ('a -> 'a ft_ref)

let return x = fun () -> (x, return)

let get r = fst (r ())

let set r x = snd (r ()) x

let bind r f = f (get r)

let () =
  let r = return 42 in
  let r2 = set r 100 in
  Printf.printf "Initial: %d\n" (get r);   (* Output: 42 *)
  Printf.printf "After set: %d\n" (get r2) (* Output: 100 *)
