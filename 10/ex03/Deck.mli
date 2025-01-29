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

module Deck : sig
  type t

  (** Creates a new deck of 52 cards in random order. *)
  val newDeck : unit -> t

  (** Returns a list of the string representations of each card in the deck. *)
  val toStringList : t -> string list

  (** Returns a list of the verbose string representations of each card in the deck. *)
  val toStringListVerbose : t -> string list

  (** Draws the first card from the deck.
      @param deck the deck from which to draw
      @return a tuple (card, remaining_deck)
      @raise Failure if the deck is empty
  *)
  val drawCard : t -> Card.t * t
end

