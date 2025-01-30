type 'a ft_ref = { mutable contents: 'a }

let return x = { contents = x }

let get r = r.contents

let set r x = r.contents <- x

let bind r f = f (get r)

let () =
  let r = return 42 in
  Printf.printf "Initial value: %d\n" (get r);
  set r 100;
  Printf.printf "After set: %d\n" (get r);
  let r2 = bind r (fun x -> return (x * 2)) in
  Printf.printf "After bind: %d\n" (get r2);;