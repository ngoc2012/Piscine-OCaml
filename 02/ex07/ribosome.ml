type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide =
  {
    p: phosphate;
    d: deoxyribose;
    n: nucleobase
  }
type aminoacid =
  | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly
  | His | Ile | Leu | Lys | Met | Phe | Pro | Ser
  | Thr | Trp | Tyr | Val | Stop

type helix = nucleotide list
type rna = nucleobase list
type protein = aminoacid list

let generate_nucleotide c =
  let char_to_n = function
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
    let n_to_string = function
      | A -> "A"
      | C -> "C"
      | T -> "T"
      | U -> "U"
      | G -> "G"
      | _ -> " "
    in
    match hx with
      | [] -> acc
      | h :: t ->
        aux t (acc ^ (n_to_string h.n))
  in
  aux hx ""

let rec rev li lo = match li with
  | [] -> lo
  | h :: t -> rev t (h :: lo)

let complementary_helix (hx: helix) : helix =
  let rec aux hx acc =
    let n_to_c = function
      | A -> 'T'
      | T -> 'A'
      | C -> 'G'
      | G -> 'C'
      | _ -> 'N'
    in
    match hx with
    | [] -> acc
    | h :: t ->
      aux t ((generate_nucleotide (n_to_c h.n)) :: acc)
  in
  rev (aux hx []) []

let generate_rna (hx: helix) : rna =
  let rec aux hx acc =
    let pairing = function
      | A -> U
      | T -> A
      | C -> G
      | G -> C
      | _ -> None
    in
    match hx with
    | [] -> acc
    | [t] -> (pairing t.n) :: acc
    | h :: t ->
      aux t ((pairing h.n) :: acc)
  in
  rev (aux hx []) []

let rna_to_string (r: rna) =
  let rec aux r acc =
    let n_to_string = function
      | A -> "A"
      | C -> "C"
      | T -> "T"
      | U -> "U"
      | G -> "G"
      | _ -> " "
    in
    match r with
      | [] -> acc
      | h :: t ->
        aux t (acc ^ (n_to_string h))
  in
  aux r ""

let generate_bases_triplets (r: rna) : (nucleobase * nucleobase * nucleobase) list =
  let rec aux r acc = match r with
    | a :: b :: c :: rest -> aux rest ((a, b, c) :: acc)
    | _ -> acc
  in
  rev (aux r []) []

let string_of_protein = function
    | Ala -> "Ala"
    | Arg -> "Arg"
    | Asn -> "Asn"
    | Asp -> "Asp"
    | Cys -> "Cys"
    | Gln -> "Gln"
    | Glu -> "Glu"
    | Gly -> "Gly"
    | His -> "His"
    | Ile -> "Ile"
    | Leu -> "Leu"
    | Lys -> "Lys"
    | Met -> "Met"
    | Phe -> "Phe"
    | Pro -> "Pro"
    | Ser -> "Ser"
    | Thr -> "Thr"
    | Trp -> "Trp"
    | Tyr -> "Tyr"
    | Val -> "Val"
    | Stop -> "Stop"

let print_protein (p: protein) =
  let rec aux p acc = match p with
    | [] -> acc
    | h :: t -> aux t (acc ^ (string_of_protein h))
  in
  print_endline (aux p "")

let decode_arn (r: rna) : protein =
  let triplet_to_aminoacid = function
    | (U, A, A) | (U, A, G) | (U, G, A) -> Stop
    | (G, C, A) | (G, C, C) | (G, C, G) | (G, C, U) -> Ala
    | (A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U) -> Arg
    | (A, A, C) | (A, A, U) -> Asn
    | (G, A, C) | (G, A, U) -> Asp
    | (U, G, C) | (U, G, U) -> Cys
    | (C, A, A) | (C, A, G) -> Gln
    | (G, A, A) | (G, A, G) -> Glu
    | (G, G, A) | (G, G, C) | (G, G, G) | (G, G, U) -> Gly
    | (C, A, C) | (C, A, U) -> His
    | (A, U, A) | (A, U, C) | (A, U, U) -> Ile
    | (C, U, A) | (C, U, C) | (C, U, G) | (C, U, U) | (U, U, A) | (U, U, G) -> Leu
    | (A, A, A) | (A, A, G) -> Lys
    | (A, U, G) -> Met
    | (U, U, C) | (U, U, U) -> Phe
    | (C, C, C) | (C, C, A) | (C, C, G) | (C, C, U) -> Pro
    | (U, C, A) | (U, C, C) | (U, C, G) | (U, C, U) | (A, G, U) | (A, G, C) -> Ser
    | (A, C, A) | (A, C, C) | (A, C, G) | (A, C, U) -> Thr
    | (U, G, G) -> Trp
    | (U, A, C) | (U, A, U) -> Tyr
    | (G, U, A) | (G, U, C) | (G, U, G) | (G, U, U) -> Val
    | _ -> Stop
  in
  let rec aux triplets acc = match triplets with
    | [] -> acc
    | h :: t ->
      let p = triplet_to_aminoacid h in
      if p = Stop then acc else aux t (p :: acc)
  in
  rev (aux (generate_bases_triplets r) []) []

let () =
  let hx = generate_helix 100 in
  print_endline (helix_to_string hx);
  print_endline (helix_to_string (complementary_helix hx));
  let r = generate_rna hx in
  print_endline (rna_to_string r);;
