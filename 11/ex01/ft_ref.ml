type 'a ft_ref = { value: 'a; update: 'a -> 'a ft_ref }

let return x = { value = x; update = (fun new_x -> return new_x) }

let get r = r.value

let set r x = r.update x

let bind r f = f (get r)


let () =
  let r = return 42 in
  Printf.printf "Initial value: %d\n" (get r);
  set r 100;
  Printf.printf "After set: %d\n" (get r);
  let r2 = bind r (fun x -> return (x * 2)) in
  Printf.printf "After bind: %d\n" (get r2);;