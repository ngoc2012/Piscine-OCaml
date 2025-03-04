module Color : sig
  type t = Spade | Heart | Diamond | Club
  val all : t list
  val toString : t -> string
  val toStringVerbose : t -> string
end

module Value : sig
  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
  val all : t list
  val toInt : t -> int
  val toString : t -> string
  val toStringVerbose : t -> string
  val next : t -> t
  val previous : t -> t
end

module Card : sig
  type t
  val newCard : Value.t -> Color.t -> t
  val getValue : t -> Value.t
  val getColor : t -> Color.t
  val toString : t -> string
  val toStringVerbose : t -> string
end

type t = Card.t list
val newDeck : unit -> t
val toStringList : t -> string list
val toStringListVerbose : t -> string list
val drawCard : t -> Card.t * t
