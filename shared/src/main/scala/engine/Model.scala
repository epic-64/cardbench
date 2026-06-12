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

/** What facing a dealt card should end up in. `Keep` leaves it as it lies; `Up`
  * and `Down` force it, flipping only when the card isn't already that way.
  */
enum TargetFacing:
  case Up, Down, Keep

  /** The concrete facing to force, or `None` to leave the card untouched. */
  def facing: Option[Facing] = this match
    case Up   => Some(Facing.Up)
    case Down => Some(Facing.Down)
    case Keep => None

object TargetFacing:
  // Lowercase on the wire, matching the authored setup JSON.
  given ReadWriter[TargetFacing] = readwriter[String].bimap(
    {
      case TargetFacing.Up   => "up"
      case TargetFacing.Down => "down"
      case TargetFacing.Keep => "keep"
    },
    {
      case "up"   => TargetFacing.Up
      case "down" => TargetFacing.Down
      case "keep" => TargetFacing.Keep
      case other  => sys.error(s"Unknown target facing: $other")
    },
  )

/** A consequence of playing a card — a single move event. The vocabulary is one
  * primitive: `Deal` moves `count` cards (default 1) from the top of one stack
  * onto the top of another, forcing each dealt card into `targetFacing` (`Keep`
  * leaves it as it lies; `Up`/`Down` flip it only when it isn't already there).
  * Everything richer composes from more of these, including where a *played* card
  * ends up: since playing first drops the card on the play stack, sending it
  * onward (e.g. to a discard) is just a `Deal` out of that stack.
  */
enum Effect derives ReadWriter:
  case Deal(from: StackId, to: StackId, count: Int = 1, targetFacing: TargetFacing = TargetFacing.Keep)

/** One atomic, animatable move in a play's resolution: relocate a single card,
  * or flip one. `Engine.playSteps` emits these in order so the shell can animate
  * them one by one; `Engine.play` simply applies them all at once.
  */
enum Step:
  case Move(card: CardId, to: StackId)
  case Flip(card: CardId)

/** Authored content — the "front" of a card. Pure presentation: what a card
  * looks like and reads as. A card's *behaviour* when played lives entirely
  * apart, in the `Rulebook` (see `CardRule`), so look and rules evolve
  * independently.
  */
case class CardDef(
  id: CardDefId,
  color: String,
  title: String,
  description: String,
) derives ReadWriter

/** A physical card on the table: one instance of a definition. */
case class CardInstance(id: CardId, defId: CardDefId, facing: Facing)

/** A free-form table coordinate in board pixels. */
case class Position(x: Int, y: Int) derives ReadWriter

/** How a stack is shown on the table. `Pile` heaps its cards at one spot so only
  * the top is visible; `Row` spreads them side by side, each card individually
  * visible — for tableau-style zones like a player's built features.
  */
enum Layout:
  case Pile, Row

object Layout:
  // Lowercase on the wire, matching the authored setup JSON.
  given ReadWriter[Layout] = readwriter[String].bimap(
    {
      case Layout.Pile => "pile"
      case Layout.Row  => "row"
    },
    {
      case "pile" => Layout.Pile
      case "row"  => Layout.Row
      case other  => sys.error(s"Unknown layout: $other")
    },
  )

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
  layout: Layout = Layout.Pile,
)

/** The whole table: the single source of truth. `catalog` is the registry of
  * card kinds for rendering; `rules` is the resolved effect system, addressing
  * each kind's play behaviour by id. A kind absent from `rules` is inert.
  */
case class GameState(
  catalog: Map[CardDefId, CardDef],
  rules: Map[CardDefId, CardRule],
  stacks: List[Stack],
)

/** A move that could not be applied. Verbs return these instead of throwing. */
enum EngineError:
  case UnknownStack(id: StackId)
  case UnknownCard(id: CardId)
  case UnknownCardDef(id: CardDefId)
  case EmptyStack(id: StackId)
