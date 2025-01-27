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
    p = "P";
    d = "D";
    n = char_to_n c;
  }

type helix = nucleotide list

let generate_helix n : helix =
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

let helix_to_string (hx: helix) =
  let rec aux hx acc =
    let n_to_string n = match n with
      | A -> "A"
      | C -> "C"
      | T -> "T"
      | U -> "U"
      | G -> "G"
      | None -> " "
    in
    match hx with
      | [] -> acc
      | [t] -> acc ^ (n_to_string t.n)
      | h :: t ->
        aux t (acc ^ (n_to_string h.n))
  in
  aux hx ""

let complementary_helix (hx: helix) : helix =
  let rec rev li lo = match li with
    | [] -> lo
    | h :: t -> rev t (h :: lo)
  in
  let rec aux hx acc =
    let n_to_c n = match n with
      | A -> 'T'
      | T -> 'A'
      | C -> 'G'
      | G -> 'C'
      | _ -> 'N'
    in
    match hx with
    | [] -> acc
    | [t] -> (generate_nucleotide (n_to_c t.n)) :: acc
    | h :: t ->
      aux t ((generate_nucleotide (n_to_c h.n)) :: acc)
  in
  rev (aux hx []) []

let complementary_helix (hx: helix) : helix =
  let rec rev li lo = match li with
    | [] -> lo
    | h :: t -> rev t (h :: lo)
  in
  let rec aux hx acc =
    let n_to_c n = match n with
      | A -> 'T'
      | T -> 'A'
      | C -> 'G'
      | G -> 'C'
      | _ -> 'N'
    in
    match hx with
    | [] -> acc
    | [t] -> (generate_nucleotide (n_to_c t.n)) :: acc
    | h :: t ->
      aux t ((generate_nucleotide (n_to_c h.n)) :: acc)
  in
  rev (aux hx []) []
  
let () =
  let hx = generate_helix 100 in
  print_endline (helix_to_string hx);
  print_endline (helix_to_string (complementary_helix hx));;
