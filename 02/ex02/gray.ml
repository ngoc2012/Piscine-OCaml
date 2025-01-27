let gray n =
  if n <= 0 then print_endline "n must be greater than 0"
  else
    let rec rev li lo = match li with
      | [] -> lo
      | h :: t -> rev t (h :: lo)
    in
    let rec insert s li lo = match li with
      | [] -> lo
      | h :: t -> insert s t ((s ^ h) :: lo)
    in
    let rec aux n output = match n with
      | 1 -> output
      | _ -> aux (n - 1) (insert "1" output []);
    in
    let print strings = 
      begin
        print_string "[";
        print_string (String.concat ", " strings);
        print_endline "]";
      end
    in
    print (aux n ["0"; "1"]);

let () =
  gray 2