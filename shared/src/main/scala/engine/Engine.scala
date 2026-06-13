package engine

import engine.EngineError.*

/** The entire engine API: deterministic setup, a handful of mechanical verbs
  * (shuffle, deal, move, flip, moveStack, extractCard), and `drop` — the one
  * verb that runs the effect system, letting rules react as cards move. Each is a
  * pure `GameState => GameState` (wrapped in `Either` for the ones that can fail).
  */
object Engine:

  /** Build the starting table from authored data. Deterministic: a stack flagged
    * `shuffled` is ordered by a per-stack seeded RNG, so the same `seed`
    * reproduces the same table. Unflagged stacks keep their authored order.
    */
  def setup(catalog: CardCatalog, rulebook: Rulebook, gameSetup: GameSetup, seed: Long = 0L): GameState =
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
        facing = spec.facing,
        shuffled = spec.arrangement == Arrangement.Shuffled,
        persistent = spec.persistent,
        layout = spec.layout,
      )
    GameState(defs, rulebook.rules, stacks)

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

  /** Move a card onto a stack and let the table react: relocate it, then run the
    * effects of every rule the move triggers — and, since each effect is itself a
    * move, the rules *those* in turn trigger, cascading until nothing more fires.
    * Applies the whole cascade at once; see `dropSteps` for the step-by-step form
    * the animated shell uses.
    */
  def drop(state: GameState, card: CardId, onto: StackId, seed: Long = 0L): Either[EngineError, GameState] =
    dropSteps(state, card, onto, seed).map(applySteps(state, _))

  /** The script a drop runs: the card relocating onto `onto`, then one ordered
    * `Step` per atomic move or flip the reactions produce, cascading through the
    * rules each move in turn triggers. Threaded through `Either`, so any failure
    * (e.g. an effect dealing from an empty stack) aborts before a single step is
    * returned — a drop is all-or-nothing.
    */
  def dropSteps(state: GameState, card: CardId, onto: StackId, seed: Long = 0L): Either[EngineError, List[Step]] =
    resolveMove(state, card, onto, seed).map(_._2)

  /** The script a stack button runs: deal *up to* `count` cards from the top of
    * `from` onto `to`, each relocation cascading through the rules exactly like a
    * drop. Dealing stops once `from` runs dry, so a button asking for more cards
    * than the stack holds simply deals them all — a deal-20 button on a 5-card
    * stack deals 5. Threaded through `Either` only for the per-card failures the
    * cascade itself can raise.
    */
  def dealSteps(state: GameState, from: StackId, to: StackId, count: Int, seed: Long = 0L): Either[EngineError, List[Step]] =
    runEffect(state, Effect.Deal(from, to, count), seed, lenient = true).map(_._2)

  /** Flip a single card between Up and Down; nothing else changes. */
  def flip(state: GameState, card: CardId): Either[EngineError, GameState] =
    findCard(state, card).toRight(UnknownCard(card)).map: _ =>
      val stacks = state.stacks.map: s =>
        if s.cards.exists(_.id == card) then
          s.copy(cards = s.cards.map(c => if c.id == card then c.copy(facing = toggle(c.facing)) else c), shuffled = false)
        else s
      state.copy(stacks = stacks)

  /** Flip every card in a stack between Up and Down at once, turning the stack's
    * own `facing` with them so cards dealt in afterwards land the new way up; the
    * pile order is untouched. Like the single-card flip, this clears the
    * freshly-shuffled hint.
    */
  def flipStack(state: GameState, stack: StackId): Either[EngineError, GameState] =
    find(state, stack).map: s =>
      replaceStack(
        state,
        s.copy(cards = s.cards.map(c => c.copy(facing = toggle(c.facing))), facing = toggle(s.facing), shuffled = false),
      )

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
      val created = Stack(newStack, "", to, List(instance), facing = instance.facing)
      state.copy(stacks = dropEmpty(without :+ created))

  // ── the effect system ────────────────────────────────────────────────────
  // The whole cascade hangs off one mutually-recursive function. `resolveMove`
  // relocates a card and runs the rules that fires; each of those rules' effects
  // is more relocations, run by `runDeal`, which calls `resolveMove` again — so
  // reactions chain naturally until a move triggers nothing. All-or-nothing via
  // `Either`: the first failure aborts before any step escapes.

  /** Relocate `card` onto `to`, turning it to match the destination stack's
    * `facing` as it lands, emit the resulting "moved onto `to`" event, and run the
    * effects of every rule that fires on it. Returns the settled state and the
    * ordered steps it produced.
    */
  private def resolveMove(
    state: GameState,
    card: CardId,
    to: StackId,
    seed: Long,
  ): Either[EngineError, (GameState, List[Step])] =
    for
      instance <- findCard(state, card).toRight(UnknownCard(card))
      dest     <- find(state, to)
      moved    <- move(state, card, to)
      flips     = Option.when(instance.facing != dest.facing)(Step.Flip(card)).toList
      flipped   = flips.foldLeft(moved)((s, _) => flip(s, card).getOrElse(s))
      event     = Event.Moved(card, instance.defId, to)
      effects   = state.rules.filter(_.trigger.fires(event)).flatMap(_.effects)
      reacted  <- runEffects(flipped, effects, seed)
    yield (reacted._1, (Step.Move(card, to) :: flips) ++ reacted._2)

  /** Run a list of effects in order, threading state and accumulating steps. */
  private def runEffects(state: GameState, effects: List[Effect], seed: Long): Either[EngineError, (GameState, List[Step])] =
    effects.foldLeft[Either[EngineError, (GameState, List[Step])]](Right((state, Nil))):
      (acc, effect) =>
        acc.flatMap:
          case (s, steps) => runEffect(s, effect, seed).map((s2, more) => (s2, steps ++ more))

  /** Run one effect. A `Deal` is `count` single relocations, each resolved (and so
    * itself able to set off further reactions) by `resolveMove`; strict by default,
    * a rule effect that over-draws its source aborts the whole cascade (see
    * `runEffects`), while `lenient` — the form a stack button runs — instead stops
    * a dry deal early, dealing only what was there. A `Shuffle` reorders one stack
    * with a per-stack seed derived from `seed`, recording that seed in the step so
    * replay reproduces the order. */
  private def runEffect(
    state: GameState,
    effect: Effect,
    seed: Long,
    lenient: Boolean = false,
  ): Either[EngineError, (GameState, List[Step])] =
    effect match
      case Effect.Deal(from, to, count) =>
        List.fill(count)(()).foldLeft[Either[EngineError, (GameState, List[Step])]](Right((state, Nil))):
          (acc, _) =>
            acc.flatMap:
              case (s, steps) =>
                // The source may have vanished once its last card left (dropEmpty),
                // so when lenient a missing or empty stack alike just stops the deal.
                find(s, from).flatMap(_.cards.headOption.toRight(EmptyStack(from))) match
                  case Left(_) if lenient  => Right((s, steps))
                  case Left(err)           => Left(err)
                  case Right(top)          => resolveMove(s, top.id, to, seed).map((s2, more) => (s2, steps ++ more))
      case Effect.Shuffle(stack) =>
        val stackSeed = seed ^ stack.value.hashCode.toLong
        shuffle(state, stack, stackSeed).map(reordered => (reordered, List(Step.Shuffle(stack, stackSeed))))

  /** Run a script straight through, ignoring per-step errors the script's own
    * construction already ruled out. */
  private def applySteps(state: GameState, steps: List[Step]): GameState =
    steps.foldLeft(state): (s, step) =>
      step match
        case Step.Move(card, to)        => move(s, card, to).getOrElse(s)
        case Step.Flip(card)            => flip(s, card).getOrElse(s)
        case Step.Shuffle(stack, seed)  => shuffle(s, stack, seed).getOrElse(s)

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
