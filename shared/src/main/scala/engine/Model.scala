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

/** What a rule does when it fires. `Deal` moves `count` cards (default 1) from
  * the top of one stack onto the top of another; each dealt card takes on the
  * destination stack's `facing` as it lands (see `Stack`), and each move is
  * itself an `Event`, so it can cascade into further rules. `Shuffle` reorders
  * one stack in place. `Manual` is the escape hatch: it expresses any effect the
  * engine can't model by pausing the cascade and showing the player a prompt — the
  * player carries the effect out by hand, then marks it done, and the rest of the
  * cascade resumes against whatever table they left behind. Everything richer
  * composes from these.
  */
enum Effect derives ReadWriter:
  case Deal(from: StackId, to: StackId, count: Int = 1)
  case Shuffle(stack: StackId)
  case Manual(description: String)

/** One atomic, animatable change in a cascade: relocate a single card, flip one,
  * or reorder a stack. `Engine.dropSteps` emits these in order so the shell can
  * animate them one by one; `Engine.drop` simply applies them all at once. A
  * `Shuffle` step carries the concrete `seed` the cascade rolled, so replaying
  * the script reproduces the exact same order.
  */
enum Step:
  case Move(card: CardId, to: StackId)
  case Flip(card: CardId)
  case Shuffle(stack: StackId, seed: Long)

/** A reaction still queued to run, tagged with the `card` whose arrival triggered
  * the rule it came from (so a `Manual` can anchor its prompt on that card) and a
  * `lenient` flag — true only for a stack button's own deal, which deals *up to* a
  * count and stops if its source dries up, where a rule's deal instead aborts.
  * `Engine.step` consumes these one at a time; the queue that remains is the whole
  * continuation of a cascade as plain data, so a pause needs no closure to resume.
  */
case class Pending(card: CardId, effect: Effect, lenient: Boolean = false)

/** The result of advancing a cascade by a single effect (`Engine.step`). `Done`
  * carries the settled table. `Ran` carries the table after the effect, the
  * animatable `steps` it produced, and the `rest` of the queue to step through
  * next. `Await` is a `Manual` effect: it carries the `card` to anchor the prompt
  * on and its `description`, and the `rest` to resume with — handed *whatever table
  * the player leaves behind*, so effects after a `Manual` correctly see by-hand
  * changes. Held outside `Step` because a pause is a boundary, not a step the shell
  * animates.
  */
enum Progress:
  case Done(state: GameState)
  case Ran(state: GameState, steps: List[Step], rest: List[Pending])
  case Await(state: GameState, card: CardId, description: String, rest: List[Pending])

/** Something that just happened on the table — the signal the effect system
  * reacts to. The only kind so far: a card came to rest on a stack. A rule whose
  * `Trigger` matches the event fires its effects in response (see
  * `Engine.dropSteps`).
  */
enum Event:
  case Moved(card: CardId, defId: CardDefId, to: StackId)

/** Authored content — the "front" of a card. Pure presentation: what a card
  * looks like and reads as. A card's *behaviour* lives entirely apart, in the
  * `Rulebook` (see `Rule`), so look and rules evolve independently.
  */
case class CardDef(
  id: CardDefId,
  color: String,
  title: String,
  description: String,
) derives ReadWriter

/** A positioned, sized box inside a card, in card-local pixels measured from the
  * card's top-left corner: where one piece of card content — the title, the
  * description — sits within the base rectangle.
  */
case class CardBox(x: Int, y: Int, width: Int, height: Int) derives ReadWriter

/** The shared visual template every card front is drawn from. The base rectangle
  * is `width`×`height` pixels filled with `background`; the `title` and
  * `description` boxes place each piece of text inside it. One layout serves the
  * whole game — a card supplies only its own content and accent colour (see
  * `CardDef`), the layout says how big the card is and where the text sits.
  */
case class CardLayout(
  width: Int,
  height: Int,
  background: String,
  title: CardBox,
  description: CardBox,
) derives ReadWriter

object CardLayout:
  /** Mirrors the original fixed CSS card: a 130×180 dark surface with the title
    * across the top and the description filling the body beneath it. Used for any
    * game authored before card layouts existed, so its cards look unchanged.
    */
  val default: CardLayout = CardLayout(
    width = 130,
    height = 180,
    background = "#16161f",
    title = CardBox(x = 10, y = 10, width = 110, height = 24),
    description = CardBox(x = 10, y = 42, width = 110, height = 126),
  )

/** A physical card on the table: one instance of a definition. */
case class CardInstance(id: CardId, defId: CardDefId, facing: Facing) derives ReadWriter

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
  * `facing` is the orientation the pile presents: any card landing on the stack
  * takes it on, so cards merge into a face-up pile face up and a face-down pile
  * face down. `flipStack` turns the whole pile — cards and `facing` together.
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
  facing: Facing = Facing.Up,
  shuffled: Boolean = false,
  persistent: Boolean = false,
  layout: Layout = Layout.Pile,
) derives ReadWriter

/** The whole table: the single source of truth. `catalog` is the registry of
  * card kinds for rendering; `rules` is the effect system — the reactions the
  * engine fires as cards move (see `Engine.dropSteps`). An event matching no rule
  * passes without consequence.
  */
case class GameState(
  catalog: Map[CardDefId, CardDef],
  rules: List[Rule],
  stacks: List[Stack],
)

/** A move that could not be applied. Verbs return these instead of throwing. */
enum EngineError:
  case UnknownStack(id: StackId)
  case UnknownCard(id: CardId)
  case UnknownCardDef(id: CardDefId)
  case EmptyStack(id: StackId)
