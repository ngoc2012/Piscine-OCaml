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
  let char_to_n c = match c with
    | 'A' -> A
    | 'C' -> C
    | 'T' -> T
    | 'U' -> U
    | 'G' -> G
    | _   -> None
  in
  {
    p = "p";
    d = "d";
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
    | [] -> acc
    | h :: t ->
      let c = match h.n with
        | A -> "A"
        | C -> "C"
        | T -> "T"
        | U -> "U"
        | G -> "G"
        | None -> "None"
      in
      aux t (acc ^ (String.make 1 c))
  in
  aux helix ""

let () =
  generate_helix 10;;
