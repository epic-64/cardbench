package engine

import upickle.default.{ReadWriter, readwriter}

// ── Identity ────────────────────────────────────────────────────────────────
// Every id is an opaque String: distinct types at compile time, plain strings
// on the wire. JSON codecs live next to each so deriving the data types works.

opaque type CardDefId = String
object CardDefId:
  def apply(s: String): CardDefId = s
  extension (id: CardDefId) def value: String = id
  given ReadWriter[CardDefId] = readwriter[String].bimap(_.value, apply)

opaque type CardId = String
object CardId:
  def apply(s: String): CardId = s
  extension (id: CardId) def value: String = id
  given ReadWriter[CardId] = readwriter[String].bimap(_.value, apply)

opaque type StackId = String
object StackId:
  def apply(s: String): StackId = s
  extension (id: StackId) def value: String = id
  given ReadWriter[StackId] = readwriter[String].bimap(_.value, apply)

// ── Cards & table ─────────────────────────────────────────────────────────────

/** Which side is showing. `Up` = front (the CardDef); `Down` = the shared back. */
enum Facing:
  case Up, Down

object Facing:
  // Lowercase on the wire, matching the authored setup JSON.
  given ReadWriter[Facing] = readwriter[String].bimap(
    {
      case Facing.Up   => "up"
      case Facing.Down => "down"
    },
    {
      case "up"   => Facing.Up
      case "down" => Facing.Down
      case other  => sys.error(s"Unknown facing: $other")
    },
  )

/** A consequence of playing a card. The vocabulary is deliberately tiny: the
  * single primitive `Deal` moves `count` cards from the top of one stack onto
  * the top of another. Anything richer is composed from more of these.
  */
enum Effect derives ReadWriter:
  case Deal(from: StackId, to: StackId, count: Int)

/** Authored content — the "front" of a card. `effects` are resolved, in order,
  * when the card is played (see `Engine.play`); a card with none is inert.
  */
case class CardDef(
  id: CardDefId,
  color: String,
  title: String,
  description: String,
  effects: List[Effect] = Nil,
) derives ReadWriter

/** A physical card on the table: one instance of a definition. */
case class CardInstance(id: CardId, defId: CardDefId, facing: Facing)

/** A free-form table coordinate in board pixels. */
case class Position(x: Int, y: Int) derives ReadWriter

/** An ordered pile. `cards.head` = top of the stack. `shuffled` is true while the
  * pile sits in a freshly shuffled order with nothing drawn, added, or flipped
  * since — a hint for the UI, cleared by any verb that touches the cards.
  *
  * `persistent` marks an essential pile — a player's deck or discard — that
  * stays on the table even at zero cards, so effects can keep targeting it. A
  * non-persistent stack ceases to exist once its last card leaves.
  */
case class Stack(
  id: StackId,
  label: String,
  position: Position,
  cards: List[CardInstance],
  shuffled: Boolean = false,
  persistent: Boolean = false,
)

/** The whole table: the single source of truth. */
case class GameState(catalog: Map[CardDefId, CardDef], stacks: List[Stack])

/** A move that could not be applied. Verbs return these instead of throwing. */
enum EngineError:
  case UnknownStack(id: StackId)
  case UnknownCard(id: CardId)
  case UnknownCardDef(id: CardDefId)
  case EmptyStack(id: StackId)
