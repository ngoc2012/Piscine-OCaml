let gray n = match n with
  | n when n < 1 -> print_endline "n must be greater than 0"
  | _ ->
    let rec rev li lo = match li with
      | [] -> lo
      | h :: t -> rev t (h :: lo)
    in
    let rec prefix s li lo = match li with
      | [] -> lo
      | h :: t -> prefix s t ((s ^ h) :: lo)
    in
    let rec aux n output = match n with
      | 1 -> output
      | _ ->
        let r = prefix "1" output [] in
        let l = prefix "0" (rev output []) r in
        aux (n - 1) l
    in
    print_endline (String.concat " " (aux n ["0"; "1"]))

let () =
  gray (-1);
  gray 0;
  gray 1;
  gray 2;
  gray 3;
  gray 4
