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

  /** Pass the turn to the next player, wrapping back to the first after the last.
    * `players` is the game's player count; one or fewer players leaves the turn
    * on player 0, so a solitaire game has nothing to advance.
    */
  def endTurn(state: GameState, players: Int): GameState =
    if players <= 1 then state.copy(currentPlayer = 0)
    else state.copy(currentPlayer = (state.currentPlayer + 1) % players)

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
    * Applies the whole cascade at once. A `Manual` effect has no headless meaning —
    * there is no player to act on the prompt — so it auto-resumes against the same
    * table, a no-op pass-through. See `dropCascade`/`step` for the atomic form the
    * animated shell drives, where a manual prompt actually pauses for the player.
    */
  def drop(state: GameState, card: CardId, onto: StackId, seed: Long = 0L): Either[EngineError, GameState] =
    dropCascade(state, card, onto, seed).flatMap(runToEnd(_, seed))

  /** Begin a drop: relocate `card` onto `onto` and report it as the first advance —
    * the move's own steps plus the reactions its arrival queues. The shell animates
    * the steps then drives the queue with `step`; pausing on any `Manual` it hits.
    * Strict — a failure (e.g. an effect dealing from an empty stack) aborts the
    * whole drop before a single step escapes.
    */
  def dropCascade(state: GameState, card: CardId, onto: StackId, seed: Long = 0L): Either[EngineError, Progress] =
    resolveOne(state, card, onto, seed).map((moved, steps, triggered) => Progress.Ran(moved, steps, triggered))

  /** Begin a stack button's deal: deal *up to* `count` cards from the top of `from`
    * onto `to`, each relocation cascading through the rules exactly like a drop and
    * pausing the same way on any `Manual`. Lenient — dealing stops once `from` runs
    * dry, so a deal-20 button on a 5-card stack deals 5. Returns the first advance;
    * the shell drives the rest with `step`.
    */
  def dealCascade(state: GameState, from: StackId, to: StackId, count: Int, seed: Long = 0L): Either[EngineError, Progress] =
    step(state, List(Pending(noCard, Effect.Deal(from, to, count), lenient = true)), seed)

  /** Advance a cascade by a single effect: run the head of the queue against the
    * *given* table and report a `Progress`. The one place a cascade can be steered —
    * because the caller chooses the table to advance from, a paused cascade resumes
    * against the player's by-hand changes simply by handing back the live table.
    * Effects that produce nothing (a drained lenient deal, a deal counted out) are
    * skipped, so every `Ran` carries real steps.
    */
  def step(state: GameState, pending: List[Pending], seed: Long): Either[EngineError, Progress] =
    pending match
      case Nil => Right(Progress.Done(state))
      case Pending(card, effect, lenient) :: rest =>
        effect match
          case Effect.Manual(description) =>
            Right(Progress.Await(state, card, description, rest))
          case Effect.Shuffle(stack) =>
            val stackSeed = seed ^ stack.value.hashCode.toLong
            shuffle(state, stack, stackSeed).map(reordered => Progress.Ran(reordered, List(Step.Shuffle(stack, stackSeed)), rest))
          case Effect.Deal(_, _, count) if count <= 0 =>
            step(state, rest, seed)
          case Effect.Deal(from, to, count) =>
            // The source may have vanished once its last card left (dropEmpty), so
            // when lenient a missing or empty stack alike just stops the deal.
            find(state, from).flatMap(_.cards.headOption.toRight(EmptyStack(from))) match
              case Left(_) if lenient => step(state, rest, seed)
              case Left(err)          => Left(err)
              case Right(top) =>
                resolveOne(state, top.id, to, seed).map: (moved, steps, triggered) =>
                  // Reactions resolve before the next dealt card: the move's own
                  // triggers go ahead of this deal's remaining count and the rest.
                  Progress.Ran(moved, steps, triggered ++ (Pending(card, Effect.Deal(from, to, count - 1), lenient) :: rest))

  /** The flat step script a drop produces, manual prompts auto-resumed and their
    * steps concatenated — the headless counterpart of `dropCascade`. */
  def dropSteps(state: GameState, card: CardId, onto: StackId, seed: Long = 0L): Either[EngineError, List[Step]] =
    dropCascade(state, card, onto, seed).flatMap(collectSteps(_, seed))

  /** The flat step script a stack button produces, manual prompts auto-resumed —
    * the headless counterpart of `dealCascade`. */
  def dealSteps(state: GameState, from: StackId, to: StackId, count: Int, seed: Long = 0L): Either[EngineError, List[Step]] =
    dealCascade(state, from, to, count, seed).flatMap(collectSteps(_, seed))

  /** Relocate `card` onto `to` with no rules fired: a plain move, flipping the card
    * to match the destination stack's facing exactly as a cascade would. The shell
    * runs this for drops made *during* a manual pause, where the player is
    * arranging the table by hand and a drop must not kick off a second cascade.
    */
  def moveSteps(state: GameState, card: CardId, to: StackId): Either[EngineError, List[Step]] =
    for
      instance <- findCard(state, card).toRight(UnknownCard(card))
      dest     <- find(state, to)
    yield Step.Move(card, to) :: Option.when(instance.facing != dest.facing)(Step.Flip(card)).toList

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
  // A cascade is a queue of pending reactions, advanced one effect at a time by
  // `step` (above). A `Deal` relocates a card (via `resolveOne`) and *prepends* the
  // rules that move triggers ahead of the rest, so reactions still resolve
  // depth-first; a `Manual` stops the queue, handing the remainder back as plain
  // data. All-or-nothing via `Either`: the first failure aborts before any step
  // escapes. The headless helpers below drive `step` to the end with no player.

  // A placeholder for a pending with no triggering card — a stack button's
  // top-level deal. Only a `Manual` ever reads the tag, and only `Manual`s reached
  // through a real move's reactions (which carry that move's card) ever pause, so
  // this is never surfaced.
  private val noCard = CardId("")

  /** Relocate `card` onto `to`, turning it to match the destination stack's
    * `facing` as it lands. Returns the settled state, the move's own steps, and the
    * reactions its arrival triggers — each tagged with `card`, strict — for the
    * queue to run next. Does not recurse; `step` drives the queue.
    */
  private def resolveOne(
    state: GameState,
    card: CardId,
    to: StackId,
    seed: Long,
  ): Either[EngineError, (GameState, List[Step], List[Pending])] =
    for
      instance <- findCard(state, card).toRight(UnknownCard(card))
      dest     <- find(state, to)
      moved    <- move(state, card, to)
    yield
      val flips     = Option.when(instance.facing != dest.facing)(Step.Flip(card)).toList
      val flipped   = flips.foldLeft(moved)((s, _) => flip(s, card).getOrElse(s))
      val event     = Event.Moved(card, instance.defId, to)
      val triggered = state.rules.filter(_.trigger.fires(event)).flatMap(_.effects).map(Pending(card, _))
      (flipped, Step.Move(card, to) :: flips, triggered)

  /** Drive a cascade to its final table, auto-resuming every pause against the same
    * state — the headless reading of a `Manual`, which has no player to act on it.
    * Propagates the first failure, so a headless drop stays all-or-nothing. */
  private def runToEnd(progress: Progress, seed: Long): Either[EngineError, GameState] = progress match
    case Progress.Done(s)              => Right(s)
    case Progress.Ran(s, _, rest)      => step(s, rest, seed).flatMap(runToEnd(_, seed))
    case Progress.Await(s, _, _, rest) => step(s, rest, seed).flatMap(runToEnd(_, seed))

  /** Flatten a cascade to its full step script, auto-resuming pauses and
    * concatenating each advance's steps — the headless step list, manual prompts
    * elided. Propagates the first failure, aborting before any step escapes. */
  private def collectSteps(progress: Progress, seed: Long): Either[EngineError, List[Step]] = progress match
    case Progress.Done(_)              => Right(Nil)
    case Progress.Ran(s, steps, rest)  => step(s, rest, seed).flatMap(collectSteps(_, seed)).map(steps ++ _)
    case Progress.Await(s, _, _, rest) => step(s, rest, seed).flatMap(collectSteps(_, seed))

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
