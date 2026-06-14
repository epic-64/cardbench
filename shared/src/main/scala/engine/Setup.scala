package engine

import upickle.default.{ReadWriter, readwriter, read, writeJs}

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
  persistent: Boolean = true,                     // an essential pile: kept on the table even at 0 cards. Authored stacks are persistent; only stacks spun up dynamically during play are not.
  layout: Layout = Layout.Pile,                   // how the stack is shown: a heap (Pile) or a row of cards
  width: Option[Int] = None,                      // expected area width in cards; a layout hint, see areaWidth
  group: String = "",                             // editor-only organisation: the id of the StackGroup this stack sits in ("" = ungrouped). Has no gameplay effect.
  owner: Option[Int] = None,                      // which player owns this stack (None = shared/global); the player a StackRef.Owned resolves against
  role: String = "",                              // the slot key a player-relative rule names (e.g. "deck", "build"); "" = not role-addressable
) derives ReadWriter:
  /** How many cards wide this stack's area is expected to be, used by the layout
    * editor to size its footprint. A planning hint only — it never caps the card
    * count. A `Pile` is always one card wide; a `Row` uses its set `width`, or three.
    */
  def areaWidth: Int = layout match
    case Layout.Pile => 1
    case Layout.Row  => width.getOrElse(3)

/** Legacy: a button once carried a single action — `DealFrom` drew `count` cards
  * *into* the button's stack from `stack`; `DealTo` dealt `count` *out of* it onto
  * `stack`. Buttons now carry a full effects list instead (see `StackButton`); this
  * is kept only so games saved with the old shape still load, where `StackButton`'s
  * reader turns each action into the equivalent `Effect.Deal`.
  */
enum ButtonAction derives ReadWriter:
  case DealFrom(stack: StackId, count: Int = 1)
  case DealTo(stack: StackId, count: Int = 1)

/** A clickable control bound to a stack: a click-triggered ruleset, just like a
  * `Rule` minus the trigger. `stackId` is the stack it sits on (where it renders);
  * `effects` is the list it runs when clicked, each effect targeting a fixed stack
  * or the current player's role exactly as a rule does. The effects run leniently —
  * a deal stops when its source runs dry rather than aborting — so a "draw 4" on a
  * short deck draws what's there.
  */
case class StackButton(stackId: StackId, label: String, effects: List[Effect] = Nil)

object StackButton:
  // The new form is `{stackId, label, effects}`. A game saved before buttons held
  // effects instead has `{stackId, label, action}` — read that too, turning the
  // single action into one Deal between the button's own stack and the other.
  private def legacyEffect(on: StackId, action: ButtonAction): Effect = action match
    case ButtonAction.DealFrom(src, count) => Effect.Deal(StackRef.Fixed(src), StackRef.Fixed(on), count)
    case ButtonAction.DealTo(dst, count)   => Effect.Deal(StackRef.Fixed(on), StackRef.Fixed(dst), count)

  given ReadWriter[StackButton] = readwriter[ujson.Value].bimap(
    b =>
      ujson.Obj(
        "stackId" -> writeJs(b.stackId),
        "label"   -> ujson.Str(b.label),
        "effects" -> writeJs(b.effects),
      ),
    json =>
      val obj     = json.obj
      val stackId = read[StackId](obj("stackId"))
      val label   = obj("label").str
      val effects = obj.get("effects") match
        case Some(arr) => read[List[Effect]](arr)
        case None      => obj.get("action").toList.map(a => legacyEffect(stackId, read[ButtonAction](a)))
      StackButton(stackId, label, effects),
  )

/** A named bucket the stack editor groups stack definitions into, purely so a game
  * with many stacks stays navigable while authoring. `id` is the stable key stacks
  * reference (see `StackSpec.group`); `name` is the editable human label. Groups
  * carry no gameplay meaning — the engine ignores them entirely.
  */
case class StackGroup(id: String, name: String) derives ReadWriter

/** The starting table: the authored stacks plus any stack buttons over them.
  * `groups` is editor-only organisation for the stacks (see `StackGroup`); it never
  * affects play and defaults to empty so games authored before it load unchanged.
  */
case class GameSetup(
  stacks: List[StackSpec],
  buttons: List[StackButton] = Nil,
  groups: List[StackGroup] = Nil,
) derives ReadWriter

/** What cards exist, for rendering. */
case class CardCatalog(cards: List[CardDef]) derives ReadWriter

/** What an `Event` must match for a rule to fire. The only kind so far: a card
  * of def `card` coming to rest on stack `to`.
  */
enum Trigger derives ReadWriter:
  case Moved(card: CardDefId, to: StackRef)

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
