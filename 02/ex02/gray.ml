let gray n =
  if n <= 0 then print_endline "n must be greater than 0"
  else
    let rec rev li lo = match li with
      | [] -> lo
      | h :: t -> rev t (h :: lo)
    in
    let rec aux n acc =
      if n = 1 then acc
      else aux (n - 1) (n :: acc)
    in
    let print strings = 
      begin
        print_string "[";
        print_string (String.concat ", " strings);
        print_endline "]";
      end
    in
    print aux n ["0"; "1"]
let () =

