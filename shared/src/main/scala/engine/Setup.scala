package engine

import upickle.default.{ReadWriter, readwriter}

// Three authored artifacts — catalog, rulebook, setup. JSON is the on-disk
// format (upickle); these case classes are the schema.

/** How a stack's authored cards are arranged at setup. `Ordered` keeps them as
  * written; `Shuffled` deals them in a seeded-random order.
  */
enum Arrangement:
  case Ordered, Shuffled

object Arrangement:
  // Lowercase on the wire, matching the authored setup JSON.
  given ReadWriter[Arrangement] = readwriter[String].bimap(
    {
      case Arrangement.Ordered  => "ordered"
      case Arrangement.Shuffled => "shuffled"
    },
    {
      case "ordered"  => Arrangement.Ordered
      case "shuffled" => Arrangement.Shuffled
      case other      => sys.error(s"Unknown arrangement: $other")
    },
  )

/** How many instances of a definition to spawn into a stack. */
case class SpawnSpec(card: CardDefId, count: Int) derives ReadWriter

/** A starting pile: where it sits, how its cards face, and what fills it. */
case class StackSpec(
  id: StackId,
  label: String,
  position: Position,
  facing: Facing,             // applied to every card spawned here
  contents: List[SpawnSpec],                      // expanded into instances, in order
  arrangement: Arrangement = Arrangement.Ordered, // how the spawned cards are arranged at setup
  persistent: Boolean = false,                    // an essential pile: kept on the table even at 0 cards
  layout: Layout = Layout.Pile,                   // how the stack is shown: a heap (Pile) or a row of cards
  width: Option[Int] = None,                      // expected area width in cards; a layout hint, see areaWidth
) derives ReadWriter:
  /** How many cards wide this stack's area is expected to be, used by the layout
    * editor to size its footprint. A planning hint only — it never caps the card
    * count. Defaults by layout when unset: a `Pile` reserves one card, a `Row` three.
    */
  def areaWidth: Int = width.getOrElse:
    layout match
      case Layout.Pile => 1
      case Layout.Row  => 3

/** What a stack button does when clicked. Each action deals cards between the
  * button's own stack and one other: `DealFrom` draws `count` cards *into* the
  * button's stack from `stack`; `DealTo` deals `count` cards *out of* the
  * button's stack onto `stack`. A "draw" button on a hand is a `DealFrom` its
  * deck; a "discard" button is a `DealTo` the discard pile.
  */
enum ButtonAction derives ReadWriter:
  case DealFrom(stack: StackId, count: Int = 1)
  case DealTo(stack: StackId, count: Int = 1)

/** A clickable control bound to a stack: a labelled shortcut for a deal that the
  * player would otherwise do by dragging. `stackId` is the stack it sits on; the
  * `action` says which other stack it deals with and in which direction.
  */
case class StackButton(stackId: StackId, label: String, action: ButtonAction) derives ReadWriter

/** The starting table: the authored stacks plus any stack buttons over them. */
case class GameSetup(stacks: List[StackSpec], buttons: List[StackButton] = Nil) derives ReadWriter

/** What cards exist, for rendering. */
case class CardCatalog(cards: List[CardDef]) derives ReadWriter

/** What an `Event` must match for a rule to fire. The only kind so far: a card
  * of def `card` coming to rest on stack `to`.
  */
enum Trigger derives ReadWriter:
  case Moved(card: CardDefId, to: StackId)

  /** Whether this trigger fires on `event`. */
  def fires(event: Event): Boolean = (this, event) match
    case (Trigger.Moved(card, to), Event.Moved(_, defId, dest)) => card == defId && to == dest

/** A reaction in the effect system: when an event matches `trigger`, run its
  * `effects` in order. Held wholly apart from the `CardCatalog` so a card's
  * presentation and its behaviour are authored independently — the catalog says
  * what a card *is*, a rule says what the table *does* in response to it.
  */
case class Rule(trigger: Trigger, effects: List[Effect] = Nil) derives ReadWriter

/** The effect system as authored data: every reaction on the table. An event
  * matching no rule passes without consequence. Kept separate from the
  * `CardCatalog` so presentation and behaviour evolve independently.
  */
case class Rulebook(rules: List[Rule]) derives ReadWriter
