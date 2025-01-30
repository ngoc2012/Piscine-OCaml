type 'a ft_ref = { mutable contents: 'a }

let return x = { contents = x }

let get r = r.contents

let set r x = r.contents <- x

let bind r f : 'b ft_ref = f (get r)

let () =
  let r = return 42 in
  print_endline ("Initial value: " ^ (string_of_int (get r)));
  set r 100;
  print_endline ("After set: " ^ (string_of_int (get r)));
  let r2 = bind r (fun x -> return (x * 2)) in
  print_endline ("After bind: " ^ (string_of_int (get r2)))
