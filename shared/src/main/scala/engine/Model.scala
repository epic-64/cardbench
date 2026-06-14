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

/** How a rule names a stack. `Fixed` is a concrete stack id — the only kind games
  * had before player ownership existed, and still how global stacks (the Play zone,
  * the shared decks) are referenced. `Owned` is *player-relative*: it names a
  * `role` (a slot every player owns one of — `"deck"`, `"build"`, `"discard"`) and
  * resolves at run time to the stack with that role owned by the *current* player
  * (see `GameState.currentPlayer` and `Engine.resolveRef`). One `Owned`-referencing
  * rule serves every player, so a turn-based game needs no per-player duplicates.
  * `Same` is *host-relative*: it names a `role` and resolves to the stack with that
  * role owned by the *same* owner as the thing the effect hangs off — the stack a
  * button sits on, or the stack a triggering card landed on (see `Engine.anchorRef`).
  * So one button authored on a player's own stacks works for every player without
  * naming the current player, and a rule's effects can target the owner of wherever
  * the card came to rest. `Choice` defers the target to *play time*: the cascade
  * pauses and the player clicks a stack to fill it in (see `Progress.Choose`,
  * `Engine.applyChoice`), so a rule can let whoever's acting decide where a card goes.
  */
enum StackRef:
  case Fixed(id: StackId)
  case Owned(role: String)
  case Same(role: String)
  case Choice

object StackRef:
  // A concrete-stack reference straight from its id string — the common case, so
  // authored rules read `StackRef("play")` rather than `StackRef.Fixed(StackId("play"))`.
  def apply(id: String): StackRef = Fixed(StackId(id))

  // On the wire a `Fixed` ref is just its bare string — exactly the shape every
  // game saved before roles existed already uses, so those files still load — an
  // `Owned` ref is a small object carrying its `role`, a `Same` ref one carrying its
  // `role` under a distinct `same` key (so it can't be mistaken for an `Owned`), and
  // a `Choice` ref a small object flagged `choice`.
  given ReadWriter[StackRef] = readwriter[ujson.Value].bimap(
    {
      case StackRef.Fixed(id)   => ujson.Str(id.value)
      case StackRef.Owned(role) => ujson.Obj("role" -> ujson.Str(role))
      case StackRef.Same(role)  => ujson.Obj("same" -> ujson.Str(role))
      case StackRef.Choice      => ujson.Obj("choice" -> ujson.Bool(true))
    },
    {
      case ujson.Str(id)                                  => StackRef.Fixed(StackId(id))
      case obj: ujson.Obj if obj.value.contains("choice") => StackRef.Choice
      case obj: ujson.Obj if obj.value.contains("same")   => StackRef.Same(obj("same").str)
      case obj: ujson.Obj                                 => StackRef.Owned(obj("role").str)
      case other                                          => sys.error(s"Unknown stack reference: $other")
    },
  )

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

/** A predicate over card definitions, naming the kinds of card an effect applies to.
  * `anyOf` is an allow-list of definition ids: a card matches when its def is in the
  * list, and an *empty* list matches *every* card — the unfiltered default. A filtered
  * `Deal` moves the top-most *matching* card rather than the literal top (so "remove a
  * debt card from the deck" reaches past whatever sits above it); a filtered `MoveChosen`
  * only lets the player pick a matching card. Defaulted to empty so an effect authored
  * before filters existed matches every card exactly as it did before.
  */
case class CardFilter(anyOf: List[CardDefId] = Nil) derives ReadWriter:
  /** Whether `defId` is one of the kinds this filter names — always true for the empty
    * (match-everything) filter.
    */
  def matches(defId: CardDefId): Boolean = anyOf.isEmpty || anyOf.contains(defId)

/** A yes/no test an `Effect.If` branches on. The only kind so far: `CountAtLeast` holds
  * when the stack `where` — a fixed id, the current player's role-stack (`StackRef.Owned`),
  * or, once anchored, the host owner's (`StackRef.Same`) — holds at least `n` cards matching
  * `filter`. An unresolvable or missing `where` counts as not holding, so a misconfigured
  * condition simply takes the else branch rather than aborting the cascade.
  */
enum Condition derives ReadWriter:
  case CountAtLeast(where: StackRef, filter: CardFilter, n: Int)

/** What a rule does when it fires. `Deal` moves `count` cards (default 1) from
  * the top of one stack onto the top of another, restricted to cards matching its
  * `filter` (an empty filter, the default, matches every card and so deals the literal
  * top); each dealt card takes on the destination stack's `facing` as it lands (see
  * `Stack`), and each move is itself an `Event`, so it can cascade into further rules.
  * `Shuffle` reorders one stack in place. `Manual` is the escape hatch: it expresses any
  * effect the engine can't model by pausing the cascade and showing the player a prompt —
  * the player carries the effect out by hand, then marks it done, and the rest of the
  * cascade resumes against whatever table they left behind. `EndTurn` advances the
  * turn to the next player (see `GameState.currentPlayer`, `Engine.endTurn`), so
  * "pass the turn" is just another effect a button or rule can run. `MoveChosen`
  * lets the player pick the *card* to move — clicking the top of a pile, or a
  * specific card in a row, narrowed to cards matching its `filter` — and sends it to
  * `to`; the cascade pauses for the pick exactly as `Manual` pauses (see
  * `Progress.ChooseCard`, `Engine.applyCardChoice`), and `card` is the runtime-only slot
  * the pick fills (always empty when authored). `If` branches the cascade on a `Condition`,
  * splicing in its `thenDo` effects when the condition holds and its `elseDo` effects when
  * it does not (see `Engine.step`) — the one effect that reads the table before acting.
  * Everything richer composes from these.
  */
enum Effect derives ReadWriter:
  case Deal(from: StackRef, to: StackRef, count: Int = 1, filter: CardFilter = CardFilter())
  case Shuffle(stack: StackRef)
  case Manual(description: String)
  case EndTurn
  case MoveChosen(to: StackRef, card: Option[CardId] = None, filter: CardFilter = CardFilter())
  case If(cond: Condition, thenDo: List[Effect] = Nil, elseDo: List[Effect] = Nil)

/** One atomic, animatable change in a cascade: relocate a single card, flip one,
  * reorder a stack, or pass the turn. `Engine.dropSteps` emits these in order so the
  * shell can animate them one by one; `Engine.drop` simply applies them all at once.
  * A `Shuffle` step carries the concrete `seed` the cascade rolled, so replaying the
  * script reproduces the exact same order. `EndTurn` carries nothing — it just bumps
  * the current player when the shell reaches it.
  */
enum Step:
  case Move(card: CardId, to: StackId)
  // Like `Move`, but slots the card in at storage index `at` (clamped) rather than
  // on top — the animatable form of a hand-arranged row drop (see `Engine.moveAt`).
  case Insert(card: CardId, to: StackId, at: Int)
  case Flip(card: CardId)
  case Shuffle(stack: StackId, seed: Long)
  case EndTurn

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
  /** A cascade halted on a `StackRef.Choice`: the next effect can't resolve until
    * the player names a stack. `card` anchors the prompt (the triggering card, or
    * the empty `noCard` for a button's own effect); `slot` labels which target is
    * being chosen ("from", "to", "stack"); `rest` is the *whole* remaining queue,
    * its head still holding the unfilled `Choice`. The shell fills it with
    * `Engine.applyChoice` and steps on. Like `Await`, held outside `Step` because a
    * pause is a boundary, not a step to animate.
    */
  case Choose(state: GameState, card: CardId, slot: String, rest: List[Pending])
  /** A cascade halted on a `MoveChosen` whose card the player has yet to pick. `card`
    * anchors the prompt (the triggering card, or `noCard` for a button); `rest` is
    * the *whole* remaining queue, its head's `MoveChosen.card` still empty. The shell
    * fills it with `Engine.applyCardChoice` from whichever card the player clicks and
    * steps on. Held outside `Step`, like the other pauses, because it is a boundary.
    */
  case ChooseCard(state: GameState, card: CardId, rest: List[Pending])

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
  corners: CardCorners = CardCorners(),
) derives ReadWriter

/** The optional corner slots of a card front, each free text — typically a single
  * emoji, a cost, or a count. The top-left corner is reserved for the title, so only
  * the other three corners are slots. What each slot reads is authored per card; how
  * the slots *look* is shared across the game (see `CornerStyle`). An empty slot
  * draws nothing.
  */
case class CardCorners(
  topRight: String = "",
  bottomLeft: String = "",
  bottomRight: String = "",
) derives ReadWriter

/** The outline a corner slot is drawn with: a full circle, a plain square, or a
  * square with softened edges.
  */
enum CornerShape:
  case Circle, Square, Rounded

object CornerShape:
  // Lowercase on the wire, matching the rest of the authored layout JSON.
  given ReadWriter[CornerShape] = readwriter[String].bimap(
    {
      case CornerShape.Circle  => "circle"
      case CornerShape.Square  => "square"
      case CornerShape.Rounded => "rounded"
    },
    {
      case "circle"  => CornerShape.Circle
      case "square"  => CornerShape.Square
      case "rounded" => CornerShape.Rounded
      case other     => sys.error(s"Unknown corner shape: $other")
    },
  )

/** How a corner slot's shape is painted with the card's accent colour. `Fill` fills
  * it solid, `Outline` draws only its border (and the text) in that colour over a
  * transparent slot, and `None` drops the shape entirely, leaving bare text.
  */
enum CornerFill:
  case Fill, Outline, None

object CornerFill:
  // Lowercase on the wire, matching the rest of the authored layout JSON.
  given ReadWriter[CornerFill] = readwriter[String].bimap(
    {
      case CornerFill.Fill    => "fill"
      case CornerFill.Outline => "outline"
      case CornerFill.None    => "none"
    },
    {
      case "fill"    => CornerFill.Fill
      case "outline" => CornerFill.Outline
      case "none"    => CornerFill.None
      case other     => sys.error(s"Unknown corner fill: $other")
    },
  )

/** How every card's corner slots are drawn — one shared style across the game, set
  * in the designer. `shape` outlines the slot, `fill` says how the card's accent
  * colour paints it, and `font` is its CSS font-family. The fill colour is each
  * card's own accent (see `CardDef.color`), not authored here; the text inside each
  * slot is per-card (see `CardCorners`).
  */
case class CornerStyle(
  shape: CornerShape,
  fill: CornerFill,
  font: String,
) derives ReadWriter

/** The shared visual template every card front is drawn from. The base rectangle
  * is `width`×`height` pixels filled with `background`; the title and description
  * flow to fill it, and the four corner slots are drawn in the shared `corner`
  * style. One layout serves the whole game — a card supplies only its own content
  * and accent colour (see `CardDef`), the layout says how big the card is and how
  * the corners look.
  */
case class CardLayout(
  width: Int,
  height: Int,
  background: String,
  // Title and description text size, each a percent of the card's base font (which
  // the in-game A−/A+ control scales), so a card stays legible at any zoom. Defaulted
  // to the original 100%/80% so games saved before these were authored look unchanged.
  titleFont: Int = 100,
  descriptionFont: Int = 80,
  // Defaulted so games saved before corner styling existed (their layout has no
  // `corner` field) still load, picking up the standard corner look.
  corner: CornerStyle = CornerStyle(shape = CornerShape.Circle, fill = CornerFill.Fill, font = "inherit"),
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
  // Which player this stack belongs to (`None` = a shared/global stack like the
  // Play zone or a common deck), and its `role` — the slot key a player-relative
  // rule resolves against (see `StackRef.Owned`). Defaulted so games authored
  // before player ownership load as all-global, role-less stacks.
  owner: Option[Int] = None,
  role: String = "",
  // An optional accent (a CSS colour) the stack glows with while empty — a visual
  // cue marking a slot waiting to be filled. `None` = no glow. Defaulted so games
  // authored before glows load unchanged.
  color: Option[String] = None,
) derives ReadWriter

/** The whole table: the single source of truth. `catalog` is the registry of
  * card kinds for rendering; `rules` is the effect system — the reactions the
  * engine fires as cards move (see `Engine.dropSteps`). An event matching no rule
  * passes without consequence. `currentPlayer` is whose turn it is — a 0-based
  * index into the game's players, advanced by `Engine.endTurn`.
  */
case class GameState(
  catalog: Map[CardDefId, CardDef],
  rules: List[Rule],
  stacks: List[Stack],
  currentPlayer: Int = 0,
  players: Int = 1, // how many players take turns; `EndTurn` wraps `currentPlayer` within this
)

/** A move that could not be applied. Verbs return these instead of throwing. */
enum EngineError:
  case UnknownStack(id: StackId)
  case UnknownCard(id: CardId)
  case UnknownCardDef(id: CardDefId)
  case EmptyStack(id: StackId)
  // A player-relative reference (`StackRef.Owned`) that no stack satisfies: the
  // current player owns no stack of that role.
  case UnresolvedRole(role: String, player: Int)
  // A host-relative reference (`StackRef.Same`) that no stack satisfies: the owner of
  // the button's stack (or of the stack a triggering card landed on) owns no stack of
  // that role. `owner` is that host owner (`None` = a shared/global host).
  case UnresolvedSame(role: String, owner: Option[Int])
