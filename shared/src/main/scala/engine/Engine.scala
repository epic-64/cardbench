package engine

import engine.EngineError.*

/** The entire engine API: four verbs plus deterministic setup. Each is a pure
  * `GameState => GameState` (wrapped in `Either` for the moves that can fail).
  */
object Engine:

  /** Build the starting table from authored data. Deterministic — no shuffle. */
  def setup(catalog: CardCatalog, gameSetup: GameSetup): GameState =
    val defs = catalog.cards.map(c => c.id -> c).toMap
    val stacks = gameSetup.stacks.map: spec =>
      val instances = spec.contents
        .flatMap(spawn => List.fill(spawn.count)(spawn.card))
        .zipWithIndex
        .map((defId, i) => CardInstance(CardId(s"${spec.id.value}#$i"), defId, spec.facing))
      Stack(spec.id, spec.label, spec.position, instances)
    GameState(defs, stacks)

  /** Reorder one stack using a seeded RNG. Same seed ⇒ same order. */
  def shuffle(state: GameState, stack: StackId, seed: Long): Either[EngineError, GameState] =
    find(state, stack).map: s =>
      replaceStack(state, s.copy(cards = scala.util.Random(seed).shuffle(s.cards)))

  /** Hand out one card: move the top of `from` onto the top of `to`. */
  def deal(state: GameState, from: StackId, to: StackId): Either[EngineError, GameState] =
    for
      src    <- find(state, from)
      _      <- find(state, to)
      top    <- src.cards.headOption.toRight(EmptyStack(from))
      result <- move(state, top.id, to)
    yield result

  /** Move a specific card instance to the top of another stack. */
  def move(state: GameState, card: CardId, to: StackId): Either[EngineError, GameState] =
    for
      instance <- findCard(state, card).toRight(UnknownCard(card))
      _        <- find(state, to)
    yield
      val without = state.stacks.map(s => s.copy(cards = s.cards.filterNot(_.id == card)))
      val placed  = without.map(s => if s.id == to then s.copy(cards = instance :: s.cards) else s)
      state.copy(stacks = placed)

  /** Flip a single card between Up and Down; nothing else changes. */
  def flip(state: GameState, card: CardId): Either[EngineError, GameState] =
    findCard(state, card).toRight(UnknownCard(card)).map: _ =>
      val stacks = state.stacks.map: s =>
        s.copy(cards = s.cards.map(c => if c.id == card then c.copy(facing = toggle(c.facing)) else c))
      state.copy(stacks = stacks)

  // ── helpers ────────────────────────────────────────────────────────────────

  private def find(state: GameState, id: StackId): Either[EngineError, Stack] =
    state.stacks.find(_.id == id).toRight(UnknownStack(id))

  private def findCard(state: GameState, card: CardId): Option[CardInstance] =
    state.stacks.flatMap(_.cards).find(_.id == card)

  private def replaceStack(state: GameState, stack: Stack): GameState =
    state.copy(stacks = state.stacks.map(s => if s.id == stack.id then stack else s))

  private def toggle(f: Facing): Facing = f match
    case Facing.Up   => Facing.Down
    case Facing.Down => Facing.Up
