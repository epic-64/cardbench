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
  def setup(catalog: CardCatalog, rulebook: Rulebook, gameSetup: GameSetup, seed: Long = 0L, players: Int = 1): GameState =
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
        owner = spec.owner,
        role = spec.role,
      )
    GameState(defs, rulebook.rules, stacks, players = players)

  /** Pass the turn to the next player, wrapping back to the first after the last.
    * Reads the game's player count from the table (`GameState.players`); one or
    * fewer players leaves the turn on player 0, so a solitaire game has nothing to
    * advance. Invoked headlessly by the `EndTurn` effect (see `step`).
    */
  def endTurn(state: GameState): GameState =
    if state.players <= 1 then state.copy(currentPlayer = 0)
    else state.copy(currentPlayer = (state.currentPlayer + 1) % state.players)

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
    step(state, List(Pending(noCard, Effect.Deal(StackRef.Fixed(from), StackRef.Fixed(to), count), lenient = true)), seed)

  /** Begin a stack button's ruleset: run its authored `effects` in order — each
    * lenient (a deal that can't draw just stops rather than aborting), each
    * cascading through the rules and pausing on a `Manual` exactly like a rule's.
    * `Owned` references resolve against the current player, so one button serves
    * whoever's turn it is; `Same` references resolve against `hostOwner` — the owner
    * of the stack the button sits on — so one button authored on a player's own stacks
    * works for every player (see `anchorRef`). A `Same` that no stack satisfies aborts
    * the whole button before any effect runs, surfacing the misconfiguration rather
    * than failing silently. Returns the first advance; the shell drives the rest with
    * `step`.
    */
  def buttonCascade(state: GameState, effects: List[Effect], hostOwner: Option[Int], seed: Long = 0L): Either[EngineError, Progress] =
    anchorAll(state, effects, hostOwner).flatMap(anchored => step(state, anchored.map(e => Pending(noCard, e, lenient = true)), seed))

  /** Resume a cascade paused on a `StackRef.Choice` (`Progress.Choose`): fill the
    * first unresolved choice in the queue's head effect with the `stack` the player
    * picked, handing back a queue ready to `step`. The shell pairs this with the
    * same `rest` the pause carried. A queue whose head holds no choice (or an empty
    * one) is returned unchanged.
    */
  def applyChoice(pending: List[Pending], stack: StackId): List[Pending] = pending match
    case Pending(card, effect, lenient) :: rest => Pending(card, fillChoice(effect, stack), lenient) :: rest
    case Nil                                    => Nil

  /** Resume a cascade paused on a `MoveChosen` (`Progress.ChooseCard`): record the
    * `chosen` card the player clicked into the queue's head `MoveChosen`, handing back
    * a queue ready to `step` (which then resolves its `to` and moves the card). A head
    * that is not an unfilled `MoveChosen` (or an empty queue) is returned unchanged.
    */
  def applyCardChoice(pending: List[Pending], chosen: CardId): List[Pending] = pending match
    case Pending(card, effect, lenient) :: rest => Pending(card, fillCard(effect, chosen), lenient) :: rest
    case Nil                                    => Nil

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
          // A `MoveChosen` with no card yet pauses for the player to pick one — before
          // any stack choice on its `to`, so the card is chosen first. The whole queue
          // rides along for `applyCardChoice` to fill and the shell to step on.
          case Effect.MoveChosen(_, None) => Right(Progress.ChooseCard(state, card, pending))
          case _ =>
            firstChoice(effect) match
              // A `Choice` target can't resolve headlessly: pause and let the player name
              // the stack. The whole queue (head included, choice still unfilled) rides
              // along so `applyChoice` can fill it and the shell can step on.
              case Some(slot) => Right(Progress.Choose(state, card, slot, pending))
              case None       => stepResolved(state, card, effect, lenient, rest, seed)

  // The body of a single step once any `Choice` target has been ruled out — the
  // effect is ready to resolve against the table as authored.
  private def stepResolved(
    state: GameState,
    card: CardId,
    effect: Effect,
    lenient: Boolean,
    rest: List[Pending],
    seed: Long,
  ): Either[EngineError, Progress] =
        effect match
          case Effect.Manual(description) =>
            Right(Progress.Await(state, card, description, rest))
          case Effect.EndTurn =>
            // Passing the turn always succeeds; it carries a single bookkeeping step
            // so the animated shell applies the change to its live table too.
            Right(Progress.Ran(endTurn(state), List(Step.EndTurn), rest))
          case Effect.Shuffle(stackRef) =>
            // A shuffle has no benign "ran dry" case — every failure is a stack that
            // can't be resolved or found, a misconfiguration that always surfaces.
            resolveRef(state, stackRef) match
              case Left(err)          => Left(err)
              case Right(stack) =>
                val stackSeed = seed ^ stack.value.hashCode.toLong
                shuffle(state, stack, stackSeed).map(reordered => Progress.Ran(reordered, List(Step.Shuffle(stack, stackSeed)), rest))
          case Effect.Deal(_, _, count) if count <= 0 =>
            step(state, rest, seed)
          case Effect.Deal(fromRef, toRef, count) =>
            // Resolve both ends first; a missing source or empty stack (or, for a
            // role ref, a player who owns no such stack) just stops the deal when
            // lenient, and aborts the cascade otherwise.
            val resolved =
              for
                from <- resolveRef(state, fromRef)
                to   <- resolveRef(state, toRef)
                top  <- find(state, from).flatMap(_.cards.headOption.toRight(EmptyStack(from)))
              yield (from, to, top)
            resolved match
              case Left(err) if lenient && lenientStop(err) => step(state, rest, seed)
              case Left(err)                                => Left(err)
              case Right((from, to, top)) =>
                resolveOne(state, top.id, to, seed).map: (moved, steps, triggered) =>
                  // Reactions resolve before the next dealt card: the move's own
                  // triggers go ahead of this deal's remaining count and the rest.
                  Progress.Ran(moved, steps, triggered ++ (Pending(card, Effect.Deal(StackRef.Fixed(from), toRef, count - 1), lenient) :: rest))
          case Effect.MoveChosen(toRef, chosen) =>
            // The card was picked in `step` (`MoveChosen(_, None)` pauses there), and
            // any `Choice` on `toRef` was resolved before this point, so both ends are
            // ready. The picked card might have since left the table (an earlier effect
            // or a by-hand move) — that stops the move when lenient, aborts otherwise.
            val resolved =
              for
                chosenId <- chosen.toRight(UnknownCard(noCard))
                to       <- resolveRef(state, toRef)
                _        <- findCard(state, chosenId).toRight(UnknownCard(chosenId))
              yield (chosenId, to)
            resolved match
              case Left(err) if lenient && lenientStop(err) => step(state, rest, seed)
              case Left(err)                                => Left(err)
              case Right((chosenId, to)) =>
                resolveOne(state, chosenId, to, seed).map: (moved, steps, triggered) =>
                  Progress.Ran(moved, steps, triggered ++ rest)

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
      instance  <- findCard(state, card).toRight(UnknownCard(card))
      dest      <- find(state, to)
      moved     <- move(state, card, to)
      event      = Event.Moved(card, instance.defId, to)
      // A fired rule's `Same` references resolve against the owner of the stack the
      // card just landed on — the rule's host — so they abort here if unsatisfiable
      // rather than failing silently mid-cascade.
      firing     = state.rules.filter(r => triggerFires(state, r.trigger, event)).flatMap(_.effects)
      triggered <- anchorAll(state, firing, dest.owner)
    yield
      val flips   = Option.when(instance.facing != dest.facing)(Step.Flip(card)).toList
      val flipped = flips.foldLeft(moved)((s, _) => flip(s, card).getOrElse(s))
      (flipped, Step.Move(card, to) :: flips, triggered.map(Pending(card, _)))

  /** Drive a cascade to its final table, auto-resuming every pause against the same
    * state — the headless reading of a `Manual`, which has no player to act on it.
    * Propagates the first failure, so a headless drop stays all-or-nothing. */
  private def runToEnd(progress: Progress, seed: Long): Either[EngineError, GameState] = progress match
    case Progress.Done(s)              => Right(s)
    case Progress.Ran(s, _, rest)      => step(s, rest, seed).flatMap(runToEnd(_, seed))
    case Progress.Await(s, _, _, rest) => step(s, rest, seed).flatMap(runToEnd(_, seed))
    // No player to pick the stack/card headlessly, so the choosing effect is dropped
    // and the cascade runs on — the analogue of a `Manual` resolving to a no-op.
    case Progress.Choose(s, _, _, rest)   => step(s, rest.drop(1), seed).flatMap(runToEnd(_, seed))
    case Progress.ChooseCard(s, _, rest)  => step(s, rest.drop(1), seed).flatMap(runToEnd(_, seed))

  /** Flatten a cascade to its full step script, auto-resuming pauses and
    * concatenating each advance's steps — the headless step list, manual prompts
    * elided. Propagates the first failure, aborting before any step escapes. */
  private def collectSteps(progress: Progress, seed: Long): Either[EngineError, List[Step]] = progress match
    case Progress.Done(_)              => Right(Nil)
    case Progress.Ran(s, steps, rest)  => step(s, rest, seed).flatMap(collectSteps(_, seed)).map(steps ++ _)
    case Progress.Await(s, _, _, rest) => step(s, rest, seed).flatMap(collectSteps(_, seed))
    // Headless, the choosing effect is elided, exactly as a manual prompt is.
    case Progress.Choose(s, _, _, rest)  => step(s, rest.drop(1), seed).flatMap(collectSteps(_, seed))
    case Progress.ChooseCard(s, _, rest) => step(s, rest.drop(1), seed).flatMap(collectSteps(_, seed))

  /** Turn an authored reference into a concrete stack id. A `Fixed` ref is its id
    * verbatim (its existence is checked where it's used, as before). An `Owned` ref
    * resolves against the *current player*: the stack that player owns with the
    * named role — so one rule retargets itself to whoever's turn it is.
    */
  private def resolveRef(state: GameState, ref: StackRef): Either[EngineError, StackId] = ref match
    case StackRef.Fixed(id) => Right(id)
    case StackRef.Owned(role) =>
      state.stacks
        .find(s => s.owner.contains(state.currentPlayer) && s.role == role)
        .map(_.id)
        .toRight(UnresolvedRole(role, state.currentPlayer))
    // A `Same` ref is rewritten to a `Fixed` id where it enters the queue (see
    // `anchorRef`), so a `Same` only reaches here if it never resolved — surfaced
    // as the same error rather than passing silently.
    case StackRef.Same(role) => Left(UnresolvedSame(role, None))
    // `step` intercepts a `Choice` target before any resolve, so this is reached only
    // by a (nonsensical) `Choice` trigger — which then simply never matches.
    case StackRef.Choice => Left(UnresolvedRole("(choice)", state.currentPlayer))

  /** Resolve any `Same` reference in `ref` to a concrete `Fixed` id against `hostOwner`
    * — the stack with the named role owned by that host (the owner of the button's
    * stack, or of the stack a triggering card landed on). Every other kind of ref is
    * left untouched, to resolve as usual at step time. A `Same` the host owns no stack
    * for fails with `UnresolvedSame`, so the misconfiguration surfaces instead of
    * silently doing nothing.
    */
  private def anchorRef(state: GameState, ref: StackRef, hostOwner: Option[Int]): Either[EngineError, StackRef] = ref match
    case StackRef.Same(role) =>
      state.stacks
        .find(s => s.owner == hostOwner && s.role == role)
        .map(s => StackRef.Fixed(s.id))
        .toRight(UnresolvedSame(role, hostOwner))
    case other => Right(other)

  // Resolve the `Same` references in one effect's stack targets against `hostOwner`
  // (see `anchorRef`); `Manual` and `EndTurn` carry no target and pass through.
  private def anchorEffect(state: GameState, effect: Effect, hostOwner: Option[Int]): Either[EngineError, Effect] = effect match
    case Effect.Deal(from, to, count) =>
      for f <- anchorRef(state, from, hostOwner); t <- anchorRef(state, to, hostOwner) yield Effect.Deal(f, t, count)
    case Effect.Shuffle(stack)       => anchorRef(state, stack, hostOwner).map(Effect.Shuffle(_))
    case Effect.MoveChosen(to, card) => anchorRef(state, to, hostOwner).map(Effect.MoveChosen(_, card))
    case other                       => Right(other)

  // Resolve the `Same` references across a whole effects list against `hostOwner`,
  // short-circuiting on the first one no stack satisfies.
  private def anchorAll(state: GameState, effects: List[Effect], hostOwner: Option[Int]): Either[EngineError, List[Effect]] =
    effects.foldRight(Right(Nil): Either[EngineError, List[Effect]]): (e, acc) =>
      for rest <- acc; anchored <- anchorEffect(state, e, hostOwner) yield anchored :: rest

  // The first target in an effect deferred to the player (`StackRef.Choice`), and a
  // label for what it is — drives the play-time pause (`Progress.Choose`). `None`
  // when every target is already concrete or player-relative.
  private def firstChoice(effect: Effect): Option[String] = effect match
    case Effect.Deal(StackRef.Choice, _, _)            => Some("from")
    case Effect.Deal(_, StackRef.Choice, _)            => Some("to")
    case Effect.Shuffle(StackRef.Choice)               => Some("stack")
    // Only once the card has been picked; a card-less MoveChosen pauses in `step` first.
    case Effect.MoveChosen(StackRef.Choice, Some(_))   => Some("to")
    case _                                             => None

  // Replace the first `StackRef.Choice` target in an effect with the picked stack —
  // the resolve step `applyChoice` performs once the player has chosen.
  private def fillChoice(effect: Effect, stack: StackId): Effect = effect match
    case d @ Effect.Deal(StackRef.Choice, _, _) => d.copy(from = StackRef.Fixed(stack))
    case d @ Effect.Deal(_, StackRef.Choice, _) => d.copy(to = StackRef.Fixed(stack))
    case Effect.Shuffle(StackRef.Choice)        => Effect.Shuffle(StackRef.Fixed(stack))
    case m @ Effect.MoveChosen(StackRef.Choice, _) => m.copy(to = StackRef.Fixed(stack))
    case other                                  => other

  // Record the picked card into a `MoveChosen` — the resolve `applyCardChoice` does
  // once the player has clicked one.
  private def fillCard(effect: Effect, chosen: CardId): Effect = effect match
    case m @ Effect.MoveChosen(_, None) => m.copy(card = Some(chosen))
    case other                          => other

  /** Whether `trigger` fires on `event` against this table. The trigger's `to`
    * reference is resolved here (an `Owned` ref against the current player), so a
    * player-relative trigger only fires for the stack the current player owns; an
    * unresolvable ref simply never matches.
    */
  private def triggerFires(state: GameState, trigger: Trigger, event: Event): Boolean =
    (trigger, event) match
      case (Trigger.Moved(card, toRef), Event.Moved(_, defId, dest)) =>
        card == defId && resolveRef(state, toRef).toOption.contains(dest)

  /** Whether a lenient effect (a stack button's own, see `buttonCascade`) should stop
    * quietly on this error rather than abort. Only the benign "nothing left to act on"
    * cases qualify — a deal whose source ran dry (so "draw 4" on a 3-card deck draws 3),
    * or a chosen card that has since left the table. A reference that names no stack at
    * all (`UnresolvedRole`, `UnresolvedSame`, `UnknownStack`) is a misconfiguration and
    * surfaces even under lenience, so nothing fails silently.
    */
  private def lenientStop(err: EngineError): Boolean = err match
    case EmptyStack(_)  => true
    case UnknownCard(_) => true
    case _              => false

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
