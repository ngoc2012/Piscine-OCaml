(*
  Set functor:

  module type OrderedType = sig
    type t
    val compare : t -> t -> int
  end

  module Make : functor (Ord : OrderedType) -> Set.S
*)

module StringSet = Set.Make(String)

let () =
  let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
  StringSet.iter print_endline set;
  print_endline (StringSet.fold ( ^ ) set "")
