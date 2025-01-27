type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide =
  {
    p: phosphate;
    d: deoxyribose;
    n: nucleobase
  }
let generate_nucleotide c =
  let char_to_n c = function
    | 'A' -> A
    | 'C' -> C
    | 'T' -> T
    | 'U' -> U
    | 'G' -> G
    | _   -> None
  in
  {
    p = "P";
    d = "D";
    n = char_to_n c
  }
type helix = nucleotide list

let generate_helix n =
  let rec aux n acc = match n with
    | 0 -> acc
    | _ ->
      let c =
        let bases = ['A'; 'C'; 'T'; 'U'; 'G'] in
        List.nth bases (Random.int (List.length bases))
      in
      aux (n - 1) ((generate_nucleotide c) :: acc)
  in
  aux n []

let helix_to_string helix =
  let rec aux helix acc = match helix with
    let n_to_string n = function
      | A -> "A"
      | C -> "C"
      | T -> "T"
      | U -> "U"
      | G -> "G"
      | None -> " "
    in
    | [] -> acc
    | [t] -> acc ^ (n_to_string t.n)
    | h :: t ->
      aux t (acc ^ (n_to_string h.n))
  in
  aux helix ""

let complementary_helix helix =
  let rec aux helix acc = match helix with
    let n_to_c n =
      | A -> 'T'
      | T -> 'A'
      | C -> 'G'
      | G -> 'C'
      | _ -> 'N'
    in
    | [] -> acc
    | [t] -> acc ^ (generate_nucleotide (n_to_c t.n))
    | h :: t ->
      aux t (acc ^ (generate_nucleotide (n_to_c h.n)))
  in
  aux helix ""

let () =
  let helix = generate_helix 10 in
  print_endline (helix_to_string helix);
  print_endline (complementary_helix helix);
