# Generic Card Engine — Design Doc v0

A minimal, game-agnostic card engine for **playtesting**. It knows nothing about
Gigadev's rules — only cards, stacks, and the moves you can make on a table. The
goal is to get *something on the table you can shuffle and deal* so design
decisions can be made by playing, not arguing.

> **Scope discipline:** this is a toy table, not a game. No turns, no scoring, no
> hand, no rules engine. Just cards, stacks, and four verbs. Everything
> Gigadev-specific is built *on top of* this later.

---

## Design principles

1. **Pure core, dumb shell.** All game state and all moves live in `shared/` as
   pure immutable functions (`GameState => GameState`). The Laminar client is a
   thin renderer holding one `Var[GameState]`. This keeps the engine fully
   unit-testable without a browser.
2. **Deterministic by seed.** The only randomness is shuffle, and it takes an
   explicit `seed`. Same seed ⇒ same order. This is what makes BDD tests possible
   (see *Test style* in the repo: one high-level call, black box, no loops/ifs).
3. **Data-driven.** Cards and the starting table are *data*, not code. You author
   a card catalog and a setup; the engine instantiates them. Adding a card or
   changing the layout is editing JSON, not recompiling logic.
4. **Identity matters.** Every card on the table is a unique *instance* with its
   own id, even if ten of them share one definition. Moves act on instances.

---

## The domain model

Lives in `shared/src/main/scala/engine/`. Sketch (Scala 3, illustrative):

```scala
opaque type CardDefId  = String   // a definition, e.g. "build"
opaque type CardId     = String   // a spawned instance, unique on the table
opaque type StackId    = String   // a pile on the table

enum Facing:
  case Up, Down

/** Authored content. The "front" of a card. */
case class CardDef(
  id: CardDefId,
  color: String,        // CSS color for the card's accent/background
  title: String,
  description: String,
)

/** A physical card on the table: one instance of a definition. */
case class CardInstance(
  id: CardId,
  defId: CardDefId,
  facing: Facing,       // Up = show front; Down = show the back
)

/** An ordered pile. head = top of the stack. */
case class Stack(
  id: StackId,
  label: String,
  position: Position,   // where it sits on the table
  cards: List[CardInstance],
)

/** A table coordinate. Grid slots in v0; can become pixels later. */
case class Position(col: Int, row: Int)

/** The whole table. This is the single source of truth. */
case class GameState(
  catalog: Map[CardDefId, CardDef],   // definitions in play, for rendering
  stacks: List[Stack],
)
```

**Conventions**
- **Top of stack = `cards.head`.** Dealing and shuffling are defined against the
  head. Keep this invariant everywhere.
- **Facing is per-instance.** A face-down deck is a stack whose cards are all
  `Facing.Down`. Flipping a single card is a first-class move.
- **The back is uniform in v0.** Face-down cards render one shared back design (no
  per-card back art yet). Front rendering uses the `CardDef`'s color + title +
  description.

---

## The four verbs (operations)

The entire engine API. Each is a pure function and the single entry point a test
exercises. Invalid moves return a typed error rather than throwing (repo style:
no try/catch, prefer `Either`/`match`).

```scala
enum EngineError:
  case UnknownStack(id: StackId)
  case UnknownCard(id: CardId)
  case EmptyStack(id: StackId)

object Engine:

  /** Build the starting table from authored data. Deterministic. */
  def setup(catalog: CardCatalog, setup: GameSetup): GameState

  /** Reorder one stack using a seeded RNG. Same seed ⇒ same order. */
  def shuffle(state: GameState, stack: StackId, seed: Long): Either[EngineError, GameState]

  /** Hand out one card: move the top card of `from` onto the top of `to`. */
  def deal(state: GameState, from: StackId, to: StackId): Either[EngineError, GameState]

  /** Move a specific card instance to the top of another stack. */
  def move(state: GameState, card: CardId, to: StackId): Either[EngineError, GameState]

  /** Flip a single card between Up and Down. */
  def flip(state: GameState, card: CardId): Either[EngineError, GameState]
```

Notes:
- **`deal` is just the common case of `move`** — top of `from` → top of `to`. It
  exists because "hand out one card" is the verb you reach for most.
- **Moves preserve facing.** `deal` does *not* auto-flip; if you want "deal face
  up," compose `deal` then `flip`. Keeps each verb doing one thing.
- **Determinism of shuffle:** use `scala.util.Random(seed)` with Fisher–Yates.
  Its algorithm is identical on JVM and Scala.js, so a seed reproduces the same
  order on both — tests and client agree.

---

## The data formats

Two authored artifacts. JSON is the on-disk format (upickle `ReadWriter`, which
Cask already uses); the case classes above are the schema. Early increments may
hand-write Scala literals — JSON loading is its own later increment.

### 1. Card catalog — *what cards exist*

Defines color, title, description per the request.

```json
{
  "cards": [
    { "id": "build",    "color": "#3b82f6", "title": "Build",      "description": "Ship a feature." },
    { "id": "refactor", "color": "#22c55e", "title": "Refactor",   "description": "Trash 1 Debt." },
    { "id": "debt",     "color": "#6b7280", "title": "Tech Debt",  "description": "Dead weight." }
  ]
}
```

```scala
case class CardCatalog(cards: List[CardDef])
```

### 2. Game setup — *the starting table*

Specifies, per the request: how many instances of each card to spawn, which
stack they go in, and where multiple stacks sit.

```json
{
  "stacks": [
    {
      "id": "deck", "label": "Dev Deck",
      "position": { "col": 1, "row": 1 },
      "facing": "down",
      "contents": [
        { "card": "build",    "count": 8 },
        { "card": "refactor", "count": 4 }
      ]
    },
    {
      "id": "discard", "label": "Discard",
      "position": { "col": 3, "row": 1 },
      "facing": "up",
      "contents": []
    }
  ]
}
```

```scala
case class StackSpec(
  id: StackId,
  label: String,
  position: Position,
  facing: Facing,            // facing applied to every card spawned here
  contents: List[SpawnSpec], // expanded into instances
)
case class SpawnSpec(card: CardDefId, count: Int)
case class GameSetup(stacks: List[StackSpec])
```

**`Engine.setup` semantics**
- For each `StackSpec`, walk `contents` in order; for each `SpawnSpec`, spawn
  `count` instances of that `CardDef`, all with the stack's `facing`.
- **Instance ids are deterministic**: assigned sequentially (e.g.
  `"deck#0"`, `"deck#1"`, …) so a setup always produces the same table — vital
  for reproducible tests before any shuffle.
- Spawn order = `contents` order = initial stack order. Shuffle later if you want
  randomness; setup itself is never random.

---

## Rendering & interaction (the shell)

Minimal Laminar in `js/`. Not the engine — just enough to *see and poke* it.

- One `Var[GameState]`. Every verb is `state.update(s => Engine.verb(...).getOrElse(s))`.
- **Table view:** render each `Stack` at its `Position` (CSS grid in v0). A stack
  shows its **top card** plus a count badge; an empty stack shows an outline slot.
- **Card view:** face-up renders the front (accent color, title, description);
  face-down renders the shared back.
- **Controls for playtesting:** per stack, a *Shuffle* button and a *Deal →*
  control (pick a target stack). Clicking a card flips it. This is throwaway
  scaffolding — good enough to shuffle a deck and deal cards onto the table.

Use `.distinct` on derived signals per the repo's Laminar guidance.

---

## Where it lives

| Concern                          | Module    |
|----------------------------------|-----------|
| Model, verbs, setup, JSON schema | `shared/` |
| Table rendering + controls       | `js/`     |
| (later) persistence / serving    | `jvm/`    |

State is **client-only** for now: the whole table lives in the browser `Var`. The
server stays the existing skeleton until persistence or multiplayer is actually
needed.

---

## Build order — increments for the agent loop

Each increment is one agent iteration: a single high-level entry function and a
"done when" that's a passing BDD test (or, for UI, a visible behavior). Build in
order; each rests on the last.

| # | Goal                                                                                                                   | Entry point                        | Done when                                                                                                                                                                |
|---|------------------------------------------------------------------------------------------------------------------------|------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | **Model types** — `CardDef`, `CardInstance`, `Facing`, `Stack`, `Position`, `GameState`, ids, `EngineError`. No logic. | —                                  | Compiles on `jvm` and `js`; no warnings.                                                                                                                                 |
| 2 | **Spawn from setup**                                                                                                   | `Engine.setup`                     | A setup with two defs (counts 8 and 4) and a second empty stack yields a table with 12 instances in the first stack, 0 in the second, deterministic ids, correct facing. |
| 3 | **Shuffle (seeded)**                                                                                                   | `Engine.shuffle`                   | Same seed reproduces the same order; the stack's multiset of cards is unchanged; unknown stack ⇒ `UnknownStack`.                                                         |
| 4 | **Deal one card**                                                                                                      | `Engine.deal`                      | Top card of `from` becomes top of `to`; `from` shrinks by 1, `to` grows by 1; dealing from an empty stack ⇒ `EmptyStack`.                                                |
| 5 | **Move a specific card**                                                                                               | `Engine.move`                      | Named instance leaves its stack and lands on top of the target; unknown card ⇒ `UnknownCard`.                                                                            |
| 6 | **Flip a card**                                                                                                        | `Engine.flip`                      | A face-down card becomes face-up (and back); only that instance changes.                                                                                                 |
| 7 | **JSON formats**                                                                                                       | `CardCatalog` / `GameSetup` codecs | A catalog and a setup round-trip through JSON unchanged.                                                                                                                 |
| 8 | **Render the table**                                                                                                   | Laminar table view                 | Loading a setup shows the stacks at their positions, top card and count visible, backs for face-down.                                                                    |
| 9 | **Playtest controls**                                                                                                  | Shuffle / Deal / flip wiring       | You can shuffle a deck and deal cards onto an empty stack by clicking, in the browser.                                                                                   |

After increment 9 you have a table you can actually play on. Gigadev's rules
(requests, debt, phases) become a *separate* layer that calls these verbs — not
part of this engine.

---

## v0.1 — card effects & essential stacks

A small, opt-in step beyond the bare table, still game-agnostic: cards can carry
*authored effects*, and stacks can be marked *essential*.

- **Effects are data on a `CardDef`.** The vocabulary is one primitive,
  `Effect.Deal(from, to, count)` — move `count` cards from the top of one stack
  onto another. A `CardDef` with no effects is inert. Richer behaviour composes
  from more `Deal`s; the enum is the place to grow it.
- **`Engine.play(state, card, to)`** resolves the card's effects in order, then
  moves the played instance onto `to`. Effects are threaded through `Either`, so
  a failing effect (e.g. dealing from an empty pile) aborts the whole play and
  leaves the original state untouched — playing is all-or-nothing.
- **Persistent stacks.** A `Stack`/`StackSpec` flagged `persistent` stays on the
  table even at zero cards, so a player's deck, discard, or building zone can
  always be targeted by an effect. Ordinary stacks still vanish when emptied.

Example (the `build` card in `SampleGame`): playing it deals two Tech Debt into
the player's discard and draws one feature into the building zone, then the build
card itself lands wherever `play` is told to put it.

| #  | Goal                                  | Entry point         | Done when                                                                                              |
|----|---------------------------------------|---------------------|-------------------------------------------------------------------------------------------------------|
| 10 | **Essential (persistent) stacks**     | `persistent` flag   | An emptied persistent stack stays on the table; an ordinary one still vanishes; effects can target it. |
| 11 | **Card effects + play**               | `Engine.play`       | Playing a card resolves its `Deal` effects in order then relocates it; a failing effect rolls back.    |

---

## Deliberately out of scope for v0

- A **hand** (cards are placed on the table; no private zone).
- **Turns, phases, scoring, win conditions** — all Gigadev rules.
- **Per-card backs**, animations, drag-and-drop (clicks/buttons suffice).
- **Server-side state, multiplayer, persistence.**
- **Undo/history** (re-deriving from a seed + move log can come later if wanted).

---

## Open questions (decide while playtesting, not before)

1. **Position model** — grid slots (v0) vs. free pixel coordinates? Grid is
   enough to lay out piles; revisit if you want a freeform table.
2. **Deal target selection** — explicit target stack (v0) vs. a default "active"
   stack to speed up repeated deals?
3. **Should `setup` shuffle?** Currently no — setup is deterministic and you
   shuffle explicitly. Keep the separation unless authoring shuffled decks by
   hand gets tedious.
4. **Move log** — worth recording moves for replay/undo, or is the live `Var`
   enough for playtesting?
