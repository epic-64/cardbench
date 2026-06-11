package engine

import engine.EngineError.*

/** The entire engine API: four verbs plus deterministic setup. Each is a pure
  * `GameState => GameState` (wrapped in `Either` for the moves that can fail).
  */
object Engine:

  /** Build the starting table from authored data. Deterministic: a stack flagged
    * `shuffled` is ordered by a per-stack seeded RNG, so the same `seed`
    * reproduces the same table. Unflagged stacks keep their authored order.
    */
  def setup(catalog: CardCatalog, gameSetup: GameSetup, seed: Long = 0L): GameState =
    val defs = catalog.cards.map(c => c.id -> c).toMap
    val stacks = gameSetup.stacks.map: spec =>
      val instances = spec.contents
        .flatMap(spawn => List.fill(spawn.count)(spawn.card))
        .zipWithIndex
        .map((defId, i) => CardInstance(CardId(s"${spec.id.value}#$i"), defId, spec.facing))
      // Card ids are assigned before any shuffle, so they stay stable; only the
      // pile order changes. A per-stack seed keeps each shuffled stack independent.
      val ordered = spec.arrangement match
        case Arrangement.Shuffled => scala.util.Random(seed ^ spec.id.value.hashCode.toLong).shuffle(instances)
        case Arrangement.Ordered  => instances
      Stack(spec.id, spec.label, spec.position, ordered)
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

  /** Move a specific card instance to the top of another stack. A source stack
    * left with no cards ceases to exist.
    */
  def move(state: GameState, card: CardId, to: StackId): Either[EngineError, GameState] =
    for
      instance <- findCard(state, card).toRight(UnknownCard(card))
      _        <- find(state, to)
    yield
      val without = state.stacks.map(s => s.copy(cards = s.cards.filterNot(_.id == card)))
      val placed  = without.map(s => if s.id == to then s.copy(cards = instance :: s.cards) else s)
      state.copy(stacks = dropEmpty(placed))

  /** Flip a single card between Up and Down; nothing else changes. */
  def flip(state: GameState, card: CardId): Either[EngineError, GameState] =
    findCard(state, card).toRight(UnknownCard(card)).map: _ =>
      val stacks = state.stacks.map: s =>
        s.copy(cards = s.cards.map(c => if c.id == card then c.copy(facing = toggle(c.facing)) else c))
      state.copy(stacks = stacks)

  /** Reposition a whole stack on the board; its cards are untouched. */
  def moveStack(state: GameState, stack: StackId, to: Position): Either[EngineError, GameState] =
    find(state, stack).map(s => replaceStack(state, s.copy(position = to)))

  /** Pull one card out of its stack into a brand-new single-card stack at `to`.
    * The caller supplies the fresh `newStack` id, keeping the verb deterministic.
    */
  def extractCard(
    state: GameState,
    card: CardId,
    newStack: StackId,
    to: Position,
  ): Either[EngineError, GameState] =
    findCard(state, card).toRight(UnknownCard(card)).map: instance =>
      val without = state.stacks.map(s => s.copy(cards = s.cards.filterNot(_.id == card)))
      val created = Stack(newStack, "", to, List(instance))
      state.copy(stacks = dropEmpty(without :+ created))

  // ── helpers ────────────────────────────────────────────────────────────────

  private def find(state: GameState, id: StackId): Either[EngineError, Stack] =
    state.stacks.find(_.id == id).toRight(UnknownStack(id))

  private def findCard(state: GameState, card: CardId): Option[CardInstance] =
    state.stacks.flatMap(_.cards).find(_.id == card)

  private def replaceStack(state: GameState, stack: Stack): GameState =
    state.copy(stacks = state.stacks.map(s => if s.id == stack.id then stack else s))

  /** A stack with no cards ceases to exist. */
  private def dropEmpty(stacks: List[Stack]): List[Stack] = stacks.filter(_.cards.nonEmpty)

  private def toggle(f: Facing): Facing = f match
    case Facing.Up   => Facing.Down
    case Facing.Down => Facing.Up
