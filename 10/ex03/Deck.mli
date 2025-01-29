module Deck :
sig
  module Card : module type of Card

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
