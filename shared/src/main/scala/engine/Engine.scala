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
      Stack(
        spec.id,
        spec.label,
        spec.position,
        ordered,
        shuffled = spec.arrangement == Arrangement.Shuffled,
        persistent = spec.persistent,
      )
    GameState(defs, stacks)

  /** Reorder one stack using a seeded RNG. Same seed ⇒ same order. Marks the
    * stack `shuffled` until something touches its cards again.
    */
  def shuffle(state: GameState, stack: StackId, seed: Long): Either[EngineError, GameState] =
    find(state, stack).map: s =>
      replaceStack(state, s.copy(cards = scala.util.Random(seed).shuffle(s.cards), shuffled = true))

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
      // Both ends are touched, so neither is "freshly shuffled" any more.
      val without = state.stacks.map: s =>
        if s.cards.exists(_.id == card) then s.copy(cards = s.cards.filterNot(_.id == card), shuffled = false)
        else s
      val placed = without.map(s => if s.id == to then s.copy(cards = instance :: s.cards, shuffled = false) else s)
      state.copy(stacks = dropEmpty(placed))

  /** Play a card: resolve its definition's effects in order, then move the
    * played instance onto `to`. Applies the whole script at once; see
    * `playSteps` for the step-by-step form the animated shell uses.
    */
  def play(state: GameState, card: CardId, to: StackId): Either[EngineError, GameState] =
    playSteps(state, card, to).map(applySteps(state, _))

  /** The script a play would run: one ordered `Step` per atomic move or flip,
    * ending with the played card moving onto `to`. The whole thing is threaded
    * through `Either`, so any failure (e.g. an effect dealing from an empty
    * stack) aborts before a single step is returned — playing is all-or-nothing.
    */
  def playSteps(state: GameState, card: CardId, to: StackId): Either[EngineError, List[Step]] =
    for
      instance <- findCard(state, card).toRight(UnknownCard(card))
      cardDef  <- state.catalog.get(instance.defId).toRight(UnknownCardDef(instance.defId))
      _        <- find(state, to)
      resolved <- cardDef.effects.foldLeft[Either[EngineError, (GameState, List[Step])]](Right((state, Nil))):
                    (acc, effect) =>
                      acc.flatMap:
                        case (s, steps) => dealSteps(s, effect).map((s2, more) => (s2, steps ++ more))
    yield resolved._2 :+ Step.Move(card, to)

  /** Flip a single card between Up and Down; nothing else changes. */
  def flip(state: GameState, card: CardId): Either[EngineError, GameState] =
    findCard(state, card).toRight(UnknownCard(card)).map: _ =>
      val stacks = state.stacks.map: s =>
        if s.cards.exists(_.id == card) then
          s.copy(cards = s.cards.map(c => if c.id == card then c.copy(facing = toggle(c.facing)) else c), shuffled = false)
        else s
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
      val without = state.stacks.map: s =>
        if s.cards.exists(_.id == card) then s.copy(cards = s.cards.filterNot(_.id == card), shuffled = false)
        else s
      val created = Stack(newStack, "", to, List(instance))
      state.copy(stacks = dropEmpty(without :+ created))

  // ── helpers ────────────────────────────────────────────────────────────────

  /** The steps one effect contributes, plus the state after running them (so the
    * next effect sees updated stack tops). `Deal` is `count` single deals; each
    * emits a `Move`, and a `Flip` too when `reveal` turns a face-down card up.
    */
  private def dealSteps(state: GameState, effect: Effect): Either[EngineError, (GameState, List[Step])] =
    effect match
      case Effect.Deal(from, to, count, reveal) =>
        List.fill(count)(()).foldLeft[Either[EngineError, (GameState, List[Step])]](Right((state, Nil))):
          (acc, _) =>
            acc.flatMap:
              case (s, steps) =>
                for
                  src   <- find(s, from)
                  top   <- src.cards.headOption.toRight(EmptyStack(from))
                  dealt <- deal(s, from, to)
                yield
                  val flips = if reveal && top.facing == Facing.Down then List(Step.Flip(top.id)) else Nil
                  val next  = flips.foldLeft(dealt)((st, _) => flip(st, top.id).getOrElse(st))
                  (next, steps ++ (Step.Move(top.id, to) :: flips))

  /** Run a script straight through, ignoring per-step errors the script's own
    * construction already ruled out. */
  private def applySteps(state: GameState, steps: List[Step]): GameState =
    steps.foldLeft(state): (s, step) =>
      step match
        case Step.Move(card, to) => move(s, card, to).getOrElse(s)
        case Step.Flip(card)     => flip(s, card).getOrElse(s)

  private def find(state: GameState, id: StackId): Either[EngineError, Stack] =
    state.stacks.find(_.id == id).toRight(UnknownStack(id))

  private def findCard(state: GameState, card: CardId): Option[CardInstance] =
    state.stacks.flatMap(_.cards).find(_.id == card)

  private def replaceStack(state: GameState, stack: Stack): GameState =
    state.copy(stacks = state.stacks.map(s => if s.id == stack.id then stack else s))

  /** A stack with no cards ceases to exist — unless it is `persistent`, an
    * essential pile that stays on the table so effects can keep targeting it.
    */
  private def dropEmpty(stacks: List[Stack]): List[Stack] =
    stacks.filter(s => s.cards.nonEmpty || s.persistent)

  private def toggle(f: Facing): Facing = f match
    case Facing.Up   => Facing.Down
    case Facing.Down => Facing.Up
