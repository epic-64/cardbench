package client

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import scala.scalajs.js
import engine.*

/** Throwaway playtest shell: one `Var[GameState]`, every verb an `update`.
  *
  * The board is free-form. Drag a card to split it off onto empty space (a new
  * stack) or drop it on another stack to combine. Drag a stack's ⠿ handle to
  * move the whole pile. Click a card to flip it.
  *
  * Cards use HTML5 drag-and-drop (its drop targets give us "combine onto stack"
  * for free). Stack moving uses pointer events instead: the stack follows the
  * cursor live and commits the exact pixel on release — no drag threshold, no
  * ghost-vs-drop mismatch.
  */
object GameView:

  // A card mid-drag, with the grab offset within it so the drop lands without
  // jumping. JS is single-threaded, so plain mutable holders are safe here.
  private final case class CardDrag(id: CardId, offX: Int, offY: Int)
  // A stack mid-move: the live DOM element plus the grab offset within it.
  private final case class StackDrag(el: dom.html.Element, grabX: Int, grabY: Int)
  // A cascade halted on a `Manual` effect: the card the prompt hangs off, its text,
  // and the continuation as plain data — the reactions still queued, plus the seed
  // the play rolled, so resuming is just another `Engine.step` from the live table.
  private final case class ManualPrompt(card: CardId, description: String, rest: List[engine.Pending], seed: Long)
  // A cascade halted on a `StackRef.Choice`: the card the prompt anchors to, the
  // `slot` being chosen ("from"/"to"/"stack") for the prompt wording, and the
  // continuation as plain data — the queue (its head's choice still unfilled) plus
  // the play's seed, so resuming is `Engine.applyChoice` then another `Engine.step`.
  private final case class ChoicePrompt(card: CardId, slot: String, rest: List[engine.Pending], seed: Long)
  // A cascade halted on a `MoveChosen`: the card the prompt anchors to, and the
  // continuation as plain data — the queue (its head's card still unpicked) plus the
  // seed, so resuming is `Engine.applyCardChoice` then another `Engine.step`.
  private final case class CardChoicePrompt(card: CardId, rest: List[engine.Pending], seed: Long)

  def view(definition: GameDefinition, onBack: () => Unit): Element =
    // A fresh table from the authored data; the saved live table (if a game is in
    // progress) only restores the stacks onto it, so the catalog and rules always
    // come from the current definition even after an edit.
    def freshSetup(): GameState =
      Engine.setup(definition.catalog, definition.rulebook, definition.setup, System.currentTimeMillis(), definition.players)
    val fresh   = freshSetup()
    val initial = GameStore
      .loadGame(definition.id)
      .map(saved => fresh.copy(stacks = saved.stacks, currentPlayer = saved.currentPlayer))
      .getOrElse(fresh)
    val state   = Var(initial)
    val catalog = fresh.catalog
    // The on-table card width in board pixels, from the shared layout; used to fan
    // pulled cards out just clear of their source and to anchor the manual prompt.
    val cardWidth = definition.layout.width

    var dragCard: Option[CardDrag]   = None
    var dragStack: Option[StackDrag] = None
    // Supplies fresh ids for stacks split off the board. Seeded past any loose
    // stack already in a restored table, so a split can't reuse a live id.
    var looseSeq = initial.stacks
      .map(_.id.value)
      .collect { case id if id.startsWith("loose#") => id.stripPrefix("loose#").toIntOption }
      .flatten
      .maxOption
      .getOrElse(0)
    var animating                    = false // true while a play's step script is running
    // A floating, zoom-scaled card preview shown during a card drag (with the grab
    // offset in screen px), so the dragged card matches its on-table size at any
    // zoom — the native drag image can't, as it ignores the board's transform.
    var dragGhost: Option[(dom.html.Element, Double, Double)] = None

    // A 1×1 transparent GIF: handed to the browser as the drag image to suppress
    // its own ghost, leaving our `dragGhost` clone as the only preview.
    val blankDragImage =
      val img = dom.document.createElement("img").asInstanceOf[dom.html.Image]
      img.src = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"
      img

    // Which stack is mid-shuffle: drives the shake animation, cleared on a timer.
    val shuffling = Var(Option.empty[StackId])

    // The manual prompt currently awaiting the player, if any: a cascade paused on
    // a `Manual` effect. While it's set the board stays interactive (the player is
    // implementing the effect by hand) but drops no longer fire rules and buttons
    // are inert — only the prompt's "Done" resumes the suspended cascade.
    val manualPause = Var(Option.empty[ManualPrompt])

    // The stack-choice prompt currently awaiting the player, if any: a cascade paused
    // on a `StackRef.Choice`. While it's set the board's normal clicks (flip, shuffle,
    // buttons, drags) are suppressed and a click on any stack instead *picks* that
    // stack to fill the choice, resuming the cascade. A "Cancel" abandons it.
    val choicePause = Var(Option.empty[ChoicePrompt])

    // The card-choice prompt awaiting the player, if any: a cascade paused on a
    // `MoveChosen`. While it's set a click on a card *picks* that card to move (the
    // top of a pile, or a specific card in a row) rather than flipping it. Cancel
    // abandons it. Never set at the same time as `choicePause` — a single effect
    // raises one pause at a time.
    val cardChoicePause = Var(Option.empty[CardChoicePrompt])

    // True while either pick prompt is up. The board's normal gestures are suppressed
    // and a click instead makes the pick (a stack for `choicePause`, a card for
    // `cardChoicePause`); read by every interaction guard below.
    def pickPending: Boolean = choicePause.now().isDefined || cardChoicePause.now().isDefined

    // The last cascade failure, if any: a reference that named no stack, an effect on a
    // missing stack — a misconfiguration the engine aborts on rather than swallowing.
    // Shown as a dismissible banner and logged to the console, so a broken button or
    // rule never just silently does nothing.
    val cascadeError = Var(Option.empty[String])

    // Which stack (if any) is open in the inspect overlay: a full view of every
    // card in the pile, with click-to-pull. Cleared when dismissed or once the
    // inspected stack empties out from under it.
    val inspecting = Var(Option.empty[StackId])

    // Camera over the free-form table: a pan offset (screen px) and a zoom, so a
    // crowded board can be scrolled and scaled. The stacks live inside `world`,
    // which carries the matching transform; the board itself is the fixed viewport.
    val pan  = Var(Position(0, 0))
    val zoom = Var(1.0)
    var panning: Option[(Double, Double, Position)] = None // pointer-down client x/y + pan at grab

    // Card text size, in rem — a multiplier on the card font driven by the A−/A+
    // toolbar controls. Independent of zoom: zoom scales the whole table
    // geometrically, this resizes only the text so cards stay readable at any zoom.
    val cardFont = Var(1.0)

    // Whether the top toolbar is shown. Hiding it hands its whole height to the
    // playing field; a small corner toggle stays put to bring it back.
    val toolbarVisible = Var(true)

    lazy val world: HtmlElement = div(
      cls := "world",
      styleAttr <-- pan.signal
        .combineWith(zoom.signal)
        .distinct
        .map((p, z) => s"transform:translate(${p.x}px,${p.y}px) scale($z)"),
      // Re-render on the stacks *or* the current player changing — the latter so a
      // card showing a %variable% (e.g. %current_player_index%) updates as the turn
      // passes, even when no card has moved.
      children <-- state.signal.map(s => (s.stacks, s.currentPlayer)).distinct.map((stacks, _) => stacks.map(stackView)),
      // The manual-effect prompt lives inside the world, anchored beside its card in
      // board coordinates, so it pans and zooms with the camera like any stack.
      child <-- manualPause.signal.map {
        case Some(prompt) => manualDialog(prompt)
        case None         => emptyNode
      },
      // The stack-choice prompt rides the camera the same way the manual one does.
      child <-- choicePause.signal.map {
        case Some(prompt) => choiceDialog(prompt)
        case None         => emptyNode
      },
      // The card-choice prompt, likewise.
      child <-- cardChoicePause.signal.map {
        case Some(prompt) => cardChoiceDialog(prompt)
        case None         => emptyNode
      },
    )

    lazy val board: HtmlElement = div(
      cls := "board",
      // While a card choice is pending every card reads as pickable (see CSS) — a
      // click on one moves it. One class on the board styles them all at once.
      cls("choosing-card") <-- cardChoicePause.signal.map(_.isDefined).distinct,
      onDragOver --> (e => onBoardDragOver(e)),
      onDrop --> (e => onBoardDrop(e)),
      onWheel --> (e => onZoom(e)),
      onPointerDown --> (e => onPanStart(e)),
      onPointerMove --> (e => onPanMove(e)),
      onPointerUp --> (e => onPanEnd(e)),
      onPointerCancel --> (_ => panning = None),
      world,
      // A cascade-failure banner, fixed to the viewport (it sits outside `world`, so it
      // doesn't pan or zoom). Dismissed by its ✕ or by the next cascade succeeding.
      child <-- cascadeError.signal.map {
        case Some(msg) =>
          div(
            cls := "cascade-error",
            span(cls := "cascade-error-text", msg),
            button(cls := "cascade-error-close", title := "Dismiss", "✕", onClick --> (_ => cascadeError.set(None))),
          )
        case None => emptyNode
      },
    )

    // Where the pointer sits in world coordinates — board pixels as the stacks see
    // them, with the current pan and zoom undone.
    def boardPoint(clientX: Double, clientY: Double): Position =
      val rect = board.ref.getBoundingClientRect()
      val p    = pan.now()
      val z    = zoom.now()
      Position(((clientX - rect.left - p.x) / z).toInt, ((clientY - rect.top - p.y) / z).toInt)

    // Pan by dragging empty board space — not a stack or card. Pointer capture
    // keeps the drag alive once the cursor leaves the viewport.
    def onPanStart(e: dom.PointerEvent): Unit =
      val onBackground = e.target == board.ref || e.target == world.ref
      if onBackground && !animating then
        board.ref.asInstanceOf[js.Dynamic].setPointerCapture(e.pointerId)
        panning = Some((e.clientX, e.clientY, pan.now()))

    def onPanMove(e: dom.PointerEvent): Unit =
      panning.foreach: (sx, sy, start) =>
        pan.set(Position((start.x + (e.clientX - sx)).toInt, (start.y + (e.clientY - sy)).toInt))

    def onPanEnd(e: dom.PointerEvent): Unit =
      panning.foreach: _ =>
        e.currentTarget.asInstanceOf[js.Dynamic].releasePointerCapture(e.pointerId)
        panning = None

    // Zoom toward the cursor on wheel, clamped, pinning the world point under the
    // pointer so the table scales around where you're looking.
    def onZoom(e: dom.WheelEvent): Unit =
      e.preventDefault()
      val rect = board.ref.getBoundingClientRect()
      val old  = zoom.now()
      val next = (if e.deltaY < 0 then old * zoomStep else old / zoomStep).max(minZoom).min(maxZoom)
      val p    = pan.now()
      val cx   = e.clientX - rect.left
      val cy   = e.clientY - rect.top
      val wx   = (cx - p.x) / old
      val wy   = (cy - p.y) / old
      pan.set(Position((cx - wx * next).toInt, (cy - wy * next).toInt))
      zoom.set(next)

    // Tear down the floating drag preview. Called from every drop handler (a drop
    // re-renders the dragged card, unmounting the source before its `dragend` can
    // fire, so we cannot rely on `dragend` alone to clean up).
    def clearDragGhost(): Unit =
      dragGhost.foreach((g, _, _) => Option(g.parentNode).foreach(_.removeChild(g)))
      dragGhost = None

    // Allow the drop, and steer the floating drag preview so it tracks the cursor
    // (the `drag` event's own coordinates are unreliable; `dragover` is steady).
    def onBoardDragOver(e: dom.DragEvent): Unit =
      e.preventDefault()
      dragGhost.foreach: (ghost, gx, gy) =>
        ghost.style.left = s"${e.clientX - gx}px"
        ghost.style.top = s"${e.clientY - gy}px"

    // A card dropped on empty board space splits into a new stack at the point
    // under the cursor. Drops onto a stack are handled by the stack itself.
    def onBoardDrop(e: dom.DragEvent): Unit =
      clearDragGhost()
      if animating || pickPending then ()
      else
        e.preventDefault()
        val p = boardPoint(e.clientX, e.clientY)
        dragCard.foreach: c =>
          looseSeq += 1
          val newId = StackId(s"loose#$looseSeq")
          val to    = Position(p.x - c.offX, p.y - c.offY)
          state.update(s => Engine.extractCard(s, c.id, newId, to).getOrElse(s))
        dragCard = None

    def stackView(stack: Stack): Element =
      val body = stack.layout match
        case Layout.Pile => topView(stack)
        case Layout.Row  => rowView(stack)
      // Every stack gets a drag-to-move handle — even empty, single-card, or row
      // zones; the shuffle button rides along only where it makes sense.
      val controls = sideControls(stack)
      // An authored glow only lights up while the stack stands empty — a cue that
      // the slot is waiting to be filled. The colour rides in as a CSS variable the
      // glow rule reads (see `.stack-glow` in engine.css).
      val glow = stack.color.filter(_ => stack.cards.isEmpty)
      div(
        cls       := "stack",
        cls("stack-row") := (stack.layout == Layout.Row),
        cls("stack-glow") := glow.isDefined,
        cls("shuffling") <-- shuffling.signal.map(_.contains(stack.id)).distinct,
        // While a choice is pending every stack reads as pickable; a click on one
        // fills the choice and resumes the cascade (see `choicePause`/`resolveChoice`).
        cls("stack-choosable") <-- choicePause.signal.map(_.isDefined).distinct,
        styleAttr := s"left:${stack.position.x}px;top:${stack.position.y}px" + glow.fold("")(c => s";--stack-glow:$c"),
        onDragOver --> (e => e.preventDefault()),
        onDrop --> (e => onStackDrop(e, stack)),
        onClick --> (_ => choicePause.now().foreach(p => resolveChoice(p, stack.id))),
        labelView(stack),
        // The card(s) and the default controls share a relative wrapper so the
        // controls pin to the leftmost card's bottom-right corner — not the bottom
        // of the buttons row below, nor (for a horizontal stack) the rightmost card.
        div(cls := "stack-body", body, controls),
        // Authored buttons sit below the card, one per row, so they never cover
        // its title or text.
        stackButtons(stack),
      )

    // The count badge is only meaningful for a real pile; a lone card shows just
    // its label (and a loose, unlabelled card shows nothing).
    def labelView(stack: Stack): Node =
      val count = stack.cards.size
      if count > 1 then div(cls := "stack-label", s"${stack.label} ($count)".trim)
      else if stack.label.nonEmpty then div(cls := "stack-label", stack.label)
      else emptyNode

    // A vertical control column hugging the stack's bottom-right: the drag-to-move
    // handle on every stack, with a flip-all and shuffle button above it where they
    // make sense — flipping needs more than one card (a lone card flips on click),
    // shuffling additionally needs a pile.
    def sideControls(stack: Stack): Element =
      val multi      = stack.cards.size > 1
      val canShuffle = stack.layout == Layout.Pile && multi
      div(
        cls := "stack-side",
        if multi then flipButton(stack) else emptyNode,
        if multi then inspectButton(stack) else emptyNode,
        if canShuffle then shuffleButton(stack) else emptyNode,
        moveHandle(stack),
      )

    def flipButton(stack: Stack): Element =
      button(
        cls   := "stack-flip",
        title := "Flip every card in this stack",
        "👁", // 👁 eye
        onClick --> { _ =>
          if !animating && !pickPending then state.update(s => Engine.flipStack(s, stack.id).getOrElse(s))
        },
      )

    def inspectButton(stack: Stack): Element =
      button(
        cls   := "stack-inspect",
        title := "Inspect every card in this stack",
        "🔍", // 🔍 magnifying lens
        onClick --> (_ => if !animating && !pickPending then inspecting.set(Some(stack.id))),
      )

    def shuffleButton(stack: Stack): Element =
      button(
        cls   := "stack-shuffle",
        title := "Shuffle this stack",
        "⇄",
        onClick --> { _ =>
          // Mark the stack first so the re-rendered element mounts with the
          // animation class already on; a timer clears it once the shake ends.
          if !animating && !pickPending then
            shuffling.set(Some(stack.id))
            dom.window.setTimeout(() => shuffling.set(None), shuffleAnimMs)
            state.update(s => Engine.shuffle(s, stack.id, System.currentTimeMillis()).getOrElse(s))
        },
      )

    // Authored shortcuts bound to this stack: each clicks through to a deal
    // (drawing into the stack, or dealing out of it) that animates and fires rules
    // just like a drag would. A button hosted on a *role* renders on every stack of
    // that role — one per player — so a single authored control appears on each. None
    // render on loose stacks split off mid-play.
    def stackButtons(stack: Stack): Node =
      definition.setup.buttons.filter(b => hostsOn(b, stack)) match
        case Nil     => emptyNode
        case buttons => div(cls := "stack-buttons", buttons.map(buttonControl(_, stack)))

    def hostsOn(b: StackButton, stack: Stack): Boolean = b.host match
      case ButtonHost.OnStack(id)  => id == stack.id
      case ButtonHost.OnRole(role) => role.nonEmpty && stack.role == role

    def buttonControl(b: StackButton, stack: Stack): Element =
      button(
        cls := "stack-button",
        b.label,
        onClick --> (_ => runButton(b, stack)),
      )

    // A button runs its authored effects as a cascade — the same effect system the
    // rules use — pausing on any Manual just like a drop. `Owned` refs resolve against
    // the current player; `Same` refs against the owner of `stack` — the instance the
    // button was clicked on — so a role-hosted control acts on whichever player's stack
    // it sits on. Ignored while a script runs or a manual prompt is open; effects run
    // leniently, so a deal from an empty source simply resolves to nothing.
    def runButton(b: StackButton, stack: Stack): Unit =
      if !animating && manualPause.now().isEmpty && !pickPending then
        startCascade(Engine.buttonCascade(state.now(), b.effects, stack.owner, _), None)

    // ── animated drops ─────────────────────────────────────────────────────
    // A drop resolves as a script of atomic steps (Engine.dropSteps); we run it
    // one step at a time so each move/flip can animate, and ignore drops while a
    // script is in flight (see `animating`).

    def cardEl(card: CardId): Option[dom.html.Element] =
      Option(board.ref.querySelector(s"[data-card-id=\"${card.value}\"]"))
        .map(_.asInstanceOf[dom.html.Element])

    // FLIP: the card has just re-rendered at its new home; slide it visually from
    // `from` (where it sat a moment ago) to rest, so the jump reads as a glide.
    def slide(card: CardId, from: Option[dom.DOMRect], durMs: Int): Unit =
      (cardEl(card), from) match
        case (Some(el), Some(f)) =>
          val now = el.getBoundingClientRect()
          // `f`/`now` are screen px, but the transform lives inside the zoomed
          // world, which scales it — so divide by zoom to glide the true distance.
          val z = zoom.now()
          el.style.zIndex = "10"
          el.style.transition = "none"
          el.style.transform = s"translate(${(f.left - now.left) / z}px, ${(f.top - now.top) / z}px)"
          el.getBoundingClientRect() // force the start offset to register before transitioning
          el.style.transition = s"transform ${durMs}ms ease"
          el.style.transform = "translate(0px, 0px)"
          // The lift is only for the glide; clear it afterwards or the card keeps
          // a z-index of 10 and covers the stack's controls (move handle, shuffle).
          dom.window.setTimeout(
            () =>
              el.style.zIndex = ""
              el.style.transition = ""
              el.style.transform = "",
            durMs,
          )
        case _ => ()

    def flipReveal(card: CardId, durMs: Int): Unit =
      cardEl(card).foreach: el =>
        // Override the CSS animation length inline so bulk flips can run fast; the
        // JS gate below uses the same duration to advance the script.
        el.style.animationDuration = s"${durMs}ms"
        el.classList.add("card-flipping")

    // `fromOverride` lets a drop start the dropped card's glide from where the
    // user released it (the floating ghost) instead of its origin — otherwise the
    // card snaps back to its source and slides forward, a move the drag already made.
    def animateStep(step: Step, fromOverride: Option[dom.DOMRect], moveDur: Int, flipDur: Int, done: () => Unit): Unit =
      step match
        case Step.Move(card, to) =>
          val from = fromOverride.orElse(cardEl(card).map(_.getBoundingClientRect()))
          state.update(s => Engine.move(s, card, to).getOrElse(s))
          slide(card, from, moveDur)
          dom.window.setTimeout(() => done(), moveDur)
        case Step.Insert(card, to, at) =>
          val from = fromOverride.orElse(cardEl(card).map(_.getBoundingClientRect()))
          state.update(s => Engine.moveAt(s, card, to, at).getOrElse(s))
          slide(card, from, moveDur)
          dom.window.setTimeout(() => done(), moveDur)
        case Step.Flip(card) =>
          state.update(s => Engine.flip(s, card).getOrElse(s))
          flipReveal(card, flipDur)
          dom.window.setTimeout(() => done(), flipDur)
        case Step.Shuffle(stack, seed) =>
          // Mark the stack so it re-renders with the shake class on, reorder it to
          // the seed the cascade rolled, then clear the mark and continue.
          shuffling.set(Some(stack))
          state.update(s => Engine.shuffle(s, stack, seed).getOrElse(s))
          dom.window.setTimeout(() => shuffling.set(None), shuffleAnimMs)
          dom.window.setTimeout(() => done(), shuffleAnimMs)
        case Step.EndTurn =>
          // Passing the turn has no animation; apply it to the live table and move on.
          state.update(s => Engine.endTurn(s))
          done()

    // Animate one batch of steps in order, calling `onDone` once the last settles.
    // `firstFrom` overrides only the first step's glide origin (the dropped card);
    // every step that follows glides from its own real position.
    def runSteps(steps: List[Step], firstFrom: Option[dom.DOMRect], onDone: () => Unit): Unit =
      // A play that touches a whole batch of cards (a big deal, sweep, or a stack
      // migration that flips every card) drags if each card animates at full speed,
      // so once the batch crosses the threshold we shrink every move and flip to
      // keep the cascade snappy.
      val touched = steps.count(s => s.isInstanceOf[Step.Move] || s.isInstanceOf[Step.Insert] || s.isInstanceOf[Step.Flip])
      val bulk    = touched >= bulkMoveThreshold
      val moveDur = if bulk then bulkMoveAnimMs else moveAnimMs
      val flipDur = if bulk then bulkFlipAnimMs else flipAnimMs
      def loop(remaining: List[Step], fromOverride: Option[dom.DOMRect]): Unit =
        remaining match
          case Nil          => onDone()
          case step :: rest => animateStep(step, fromOverride, moveDur, flipDur, () => loop(rest, None))
      loop(steps, firstFrom)

    // Drive a cascade one advance at a time: animate the steps this effect produced,
    // then step the queue that remains. `Done` ends the play; `Await` releases the
    // board and raises the prompt — the player implements the effect by hand and
    // "Done" resumes from the table they leave behind. The queue advances from the
    // *live* table (`state.now()`), so resuming after a pause sees those by-hand changes.
    def drive(progress: Progress, firstFrom: Option[dom.DOMRect], seed: Long): Unit =
      progress match
        case Progress.Done(_) =>
          animating = false
        case Progress.Ran(_, steps, rest) =>
          runSteps(steps, firstFrom, () => continue(Engine.step(state.now(), rest, seed), None, seed))
        case Progress.Await(_, card, description, rest) =>
          animating = false
          manualPause.set(Some(ManualPrompt(card, description, rest, seed)))
        case Progress.Choose(_, card, slot, rest) =>
          // Release the board and raise the picker; a stack click resolves it.
          animating = false
          choicePause.set(Some(ChoicePrompt(card, slot, rest, seed)))
        case Progress.ChooseCard(_, card, rest) =>
          // Release the board and raise the picker; a card click resolves it.
          animating = false
          cardChoicePause.set(Some(CardChoicePrompt(card, rest, seed)))

    // Surface a cascade failure: stop the script, log it to the console, and raise the
    // dismissible banner — so a misconfigured button or rule reports plainly instead of
    // silently doing nothing.
    def report(err: EngineError): Unit =
      val msg = engineErrorMessage(err)
      dom.console.error(s"Cascade aborted: $msg")
      animating = false
      cascadeError.set(Some(msg))

    // Advance from an engine result: drive the cascade on success, raise the banner on
    // failure. Every step of every cascade funnels through here, so no abort is dropped.
    def continue(result: Either[EngineError, Progress], from: Option[dom.DOMRect], seed: Long): Unit =
      result match
        case Right(progress) => drive(progress, from, seed)
        case Left(err)       => report(err)

    // Kick off a cascade. The play's seed is rolled once here and threaded through
    // every advance so shuffles stay deterministic across the whole cascade, pauses
    // included. A failure to even begin (a bad reference) raises the banner.
    def startCascade(begin: Long => Either[EngineError, Progress], firstFrom: Option[dom.DOMRect]): Unit =
      cascadeError.set(None) // a fresh attempt clears any prior failure banner
      val seed = System.currentTimeMillis()
      begin(seed) match
        case Right(progress) => animating = true; drive(progress, firstFrom, seed)
        case Left(err)       => report(err)

    // Resume the suspended cascade against the table as the player left it, then
    // clear the prompt; the resumed advance may itself pause again, looping the same way.
    def completeManual(prompt: ManualPrompt): Unit =
      manualPause.set(None)
      animating = true
      continue(Engine.step(state.now(), prompt.rest, prompt.seed), None, prompt.seed)

    // Fill the pending choice with the stack the player clicked and resume the
    // cascade; like a manual resume, the next advance may pause again (a second
    // choice, or a manual), looping back through `drive`.
    def resolveChoice(prompt: ChoicePrompt, stack: StackId): Unit =
      choicePause.set(None)
      animating = true
      continue(Engine.step(state.now(), Engine.applyChoice(prompt.rest, stack), prompt.seed), None, prompt.seed)

    // Abandon a pending choice: the rest of its cascade is dropped, the board returns
    // to normal. The table keeps whatever the cascade settled before the pause.
    def cancelChoice(): Unit =
      choicePause.set(None)
      animating = false

    // Fill the pending card choice with the card the player clicked and resume the
    // cascade; the move's resolve (and any stack choice on its destination) plays out
    // through `drive`, which may pause again.
    def resolveCardChoice(prompt: CardChoicePrompt, card: CardId): Unit =
      cardChoicePause.set(None)
      animating = true
      continue(Engine.step(state.now(), Engine.applyCardChoice(prompt.rest, card), prompt.seed), None, prompt.seed)

    // Abandon a pending card choice, dropping the rest of its cascade.
    def cancelCardChoice(): Unit =
      cardChoicePause.set(None)
      animating = false

    // Drop a card onto a stack: relocate it and play out whatever reactions the
    // rules fire, as an animated cascade. The card just merging on top with no rule
    // to react is simply the one-step case. A drop that resolves to nothing (Left)
    // is ignored. During a manual pause the player is arranging the table by hand,
    // so a drop is a plain move that fires no rules (it must not start a second cascade).
    def dropAnimated(card: CardId, onto: StackId, from: Option[dom.DOMRect]): Unit =
      manualPause.now() match
        case Some(_) =>
          Engine.moveSteps(state.now(), card, onto).foreach: steps =>
            animating = true
            runSteps(steps, from, () => animating = false)
        case None =>
          startCascade(Engine.dropCascade(state.now(), card, onto, _), from)

    // Place a card into a row at a chosen slot with no rules fired — the rules-free
    // arrange path (a reorder within the row, or any drop during a manual pause). The
    // rule-firing counterpart is `dropCascade` with an `at` (see `onStackDrop`).
    def insertAnimated(card: CardId, onto: StackId, at: Int, from: Option[dom.DOMRect]): Unit =
      Engine.insertSteps(state.now(), card, onto, at).foreach: steps =>
        animating = true
        runSteps(steps, from, () => animating = false)

    // The storage index a card should land at when dropped into a row at `cursorX`.
    // The row's card elements run left→right in DOM order = storage *reversed*, so the
    // rightmost non-dragged card whose center sits left of the cursor is the one the
    // drop lands immediately right of visually — i.e. *at* that card's index in the
    // post-removal list, which is exactly what `Engine.moveAt` splits on. A drop left
    // of every card appends to the storage tail (the row's far-left slot).
    def rowInsertIndex(rowEl: dom.Element, stack: Stack, dragged: CardId, cursorX: Double): Int =
      val rest  = stack.cards.filterNot(_.id == dragged)
      val cards = rowEl.querySelectorAll("[data-card-id]")
      val leftId = (0 until cards.length).iterator
        .map(i => cards(i).asInstanceOf[dom.html.Element])
        .filter(el => el.getAttribute("data-card-id") != dragged.value)
        .filter: el =>
          val r = el.getBoundingClientRect()
          r.left + r.width / 2 < cursorX
        .map(el => el.getAttribute("data-card-id"))
        .toSeq
        .lastOption
      leftId.map(id => rest.indexWhere(_.id == CardId(id))).filter(_ >= 0).getOrElse(rest.size)

    // A card dropped onto a stack relocates onto it (and the rules react). Dropped
    // onto its own single-card stack it just repositions instead — so a lone card
    // can be nudged anywhere, not only by dragging it clear of the stack's bounds.
    // Dropped onto a row it lands at the slot under the cursor: a reorder within the
    // row (or any drop during a manual pause) just rearranges with no rules, while a
    // card brought in from elsewhere still fires its cascade but settles at that slot.
    def onStackDrop(e: dom.DragEvent, stack: Stack): Unit =
      // Grab the floating ghost's screen position before tearing it down, so the
      // dropped card can glide from where it was released rather than its origin.
      val released = dragGhost.map((g, _, _) => g.getBoundingClientRect())
      clearDragGhost()
      if animating || pickPending then ()
      else
        dragCard.foreach: c =>
          e.preventDefault()
          e.stopPropagation()
          val selfLone = stack.cards.size == 1 && stack.cards.exists(_.id == c.id)
          val p        = boardPoint(e.clientX, e.clientY)
          dragCard = None
          if selfLone then
            state.update(s => Engine.moveStack(s, stack.id, Position(p.x - c.offX, p.y - c.offY)).getOrElse(s))
          else
            stack.layout match
              case Layout.Row =>
                val rowEl = Option(e.currentTarget.asInstanceOf[dom.Element].querySelector(".zone-row"))
                val at    = rowEl.map(rowInsertIndex(_, stack, c.id, e.clientX)).getOrElse(0)
                if stack.cards.exists(_.id == c.id) || manualPause.now().isDefined then
                  insertAnimated(c.id, stack.id, at, released)
                else startCascade(Engine.dropCascade(state.now(), c.id, stack.id, _, Some(at)), released)
              case Layout.Pile =>
                dropAnimated(c.id, stack.id, released)

    // Pointer-driven, so movement is pixel-exact and starts immediately. We move
    // the live element directly during the drag (no re-render churn that would
    // drop the pointer capture) and commit the final position on release.
    def moveHandle(stack: Stack): Element =
      div(
        cls   := "stack-handle",
        title := "Drag to move this stack",
        "⠿",
        onPointerDown --> { e =>
          if !animating && !pickPending then
            e.preventDefault()
            val handle  = e.currentTarget.asInstanceOf[dom.Element]
            val stackEl = handle.closest(".stack").asInstanceOf[dom.html.Element]
            val grab    = boardPoint(e.clientX, e.clientY)
            // setPointerCapture keeps move/up events on the handle once the cursor
            // leaves it; it's missing from this scala-js-dom facade, so call it raw.
            handle.asInstanceOf[js.Dynamic].setPointerCapture(e.pointerId)
            dragStack = Some(StackDrag(stackEl, grab.x - stack.position.x, grab.y - stack.position.y))
        },
        onPointerMove --> { e =>
          dragStack.foreach: d =>
            val p = boardPoint(e.clientX, e.clientY)
            d.el.style.left = s"${p.x - d.grabX}px"
            d.el.style.top = s"${p.y - d.grabY}px"
        },
        onPointerUp --> { e =>
          dragStack.foreach: d =>
            val p = boardPoint(e.clientX, e.clientY)
            e.currentTarget.asInstanceOf[js.Dynamic].releasePointerCapture(e.pointerId)
            dragStack = None
            state.update(s => Engine.moveStack(s, stack.id, Position(p.x - d.grabX, p.y - d.grabY)).getOrElse(s))
        },
        onPointerCancel --> (_ => dragStack = None),
      )

    // One card instance: draggable, click-to-flip, tagged with its id so the
    // animator can find it. `depth` adds the layered-deck look; `badge` overlays
    // a mark (e.g. the shuffle hint). Shared by the pile and row layouts.
    def renderCard(stack: Stack, card: CardInstance, depth: Seq[String], badge: Node): Element =
      val startDrag = onDragStart --> { e =>
        val el    = e.currentTarget.asInstanceOf[dom.html.Element]
        val r     = el.getBoundingClientRect()
        val z     = zoom.now()
        val grabX = e.clientX - r.left // screen px within the rendered (zoomed) card
        val grabY = e.clientY - r.top
        // Store the grab offset in world units — the space `boardPoint` returns.
        dragCard = Some(CardDrag(card.id, (grabX / z).toInt, (grabY / z).toInt))
        e.dataTransfer.setData("text/plain", card.id.value)
        // The native drag image can't be scaled to the board's zoom (it ignores
        // ancestor transforms), so suppress it and float our own clone instead — a
        // copy scaled by zoom, tracking the cursor via `onBoardDragOver`.
        e.dataTransfer.asInstanceOf[js.Dynamic].setDragImage(blankDragImage, 0, 0)
        val ghost = el.cloneNode(true).asInstanceOf[dom.html.Element]
        ghost.classList.remove("card-stacked") // a single dragged card, not a pile
        ghost.style.position = "fixed"
        ghost.style.margin = "0"
        ghost.style.left = s"${e.clientX - grabX}px"
        ghost.style.top = s"${e.clientY - grabY}px"
        ghost.style.zIndex = "1000"
        ghost.style.transform = s"scale($z)"
        ghost.style.setProperty("transform-origin", "top left")
        ghost.style.setProperty("pointer-events", "none")
        dom.document.body.appendChild(ghost)
        dragGhost = Some((ghost, grabX, grabY))
        // Hide the source so the card appears to leave its stack and only the
        // floating clone shows; deferred so it doesn't disturb the drag start.
        dom.window.setTimeout(() => el.classList.add("card-dragging"), 0)
      }
      val endDrag = onDragEnd --> { e =>
        e.currentTarget.asInstanceOf[dom.html.Element].classList.remove("card-dragging")
        clearDragGhost() // fallback for a cancelled drag / drop outside any target
        dragCard = None
      }
      // A card click flips it — except while a pick is pending. During a *card* choice
      // the click instead picks this very card (stopping it from bubbling to the
      // stack); during a *stack* choice it does nothing here and bubbles up so the
      // stack handler picks the stack.
      val flip = onClick --> { e =>
        cardChoicePause.now() match
          case Some(prompt) =>
            e.stopPropagation()
            resolveCardChoice(prompt, card.id)
          case None =>
            if !animating && !pickPending then state.update(s => Engine.flip(s, card.id).getOrElse(s))
      }
      card.facing match
        case Facing.Down =>
          div(
            cls       := Seq("card", "card-back") ++ depth,
            draggable := true,
            dataAttr("card-id") := card.id.value,
            startDrag,
            endDrag,
            flip,
            badge,
          )
        case Facing.Up =>
          // Resolve %variable% tokens in the card's text against the live table, so
          // a card can show e.g. the current player; the board re-renders as it changes.
          val d = catalog.get(card.defId).map(cd => Variables.resolve(cd, state.now()))
          div(
            cls       := Seq("card", "card-front") ++ depth,
            draggable := true,
            dataAttr("card-id") := card.id.value,
            styleAttr := CardFace.accent(d.map(_.color).getOrElse("transparent")),
            CardFace.boxes(d.map(_.title).getOrElse(card.defId.value), d.map(_.description).getOrElse(""), d.map(_.corners).getOrElse(CardCorners()), definition.layout.corner.fill),
            startDrag,
            endDrag,
            flip,
            badge,
          )

    // A pile shows only its top card: a layered look for depth, plus any shuffle badge.
    def topView(stack: Stack): Element =
      stack.cards.headOption match
        case None => div(cls := "card card-empty", "—")
        case Some(card) =>
          val depth = if stack.cards.size > 1 then Seq("card-stacked") else Nil
          val badge = if stack.shuffled then div(cls := "shuffle-badge", "⇄") else emptyNode
          renderCard(stack, card, depth, badge)

    // A row zone spreads every card side by side — oldest left, newest right, so a
    // card dealt in animates into a fresh slot without nudging the others.
    def rowView(stack: Stack): Element =
      if stack.cards.isEmpty then div(cls := "card card-empty", "—")
      else div(cls := "zone-row", stack.cards.reverse.map(renderCard(stack, _, Nil, emptyNode)))

    // ── inspect overlay ──────────────────────────────────────────────────────
    // A full view of every card in a pile, opened from the lens control. Cards
    // show their front so the pile can be read regardless of facing; clicking one
    // pulls it out onto the board as a loose stack.

    // Pull a card out of the inspected stack into a fresh loose stack, cascaded
    // just clear of the source's right edge so successive pulls fan out instead of
    // landing on one spot. The overlay stays open (the grid re-renders a card shorter).
    def pullCard(stack: Stack, card: CardInstance): Unit =
      if !animating then
        looseSeq += 1
        val step  = (looseSeq % 6) * cascadeStep
        val to    = Position(stack.position.x + cardWidth + cascadeStep + step, stack.position.y + step)
        val newId = StackId(s"loose#$looseSeq")
        state.update(s => Engine.extractCard(s, card.id, newId, to).getOrElse(s))

    // One card in the inspect grid: its front, always readable, click to pull.
    def inspectCardView(stack: Stack, card: CardInstance): Element =
      val d = catalog.get(card.defId).map(cd => Variables.resolve(cd, state.now()))
      div(
        cls       := Seq("card", "card-front", "inspect-card"),
        styleAttr := CardFace.accent(d.map(_.color).getOrElse("transparent")),
        title     := "Click to pull this card out of the stack",
        CardFace.boxes(d.map(_.title).getOrElse(card.defId.value), d.map(_.description).getOrElse(""), d.map(_.corners).getOrElse(CardCorners()), definition.layout.corner.fill),
        onClick --> (_ => pullCard(stack, card)),
      )

    // The dismissable overlay for one stack. The backdrop closes on a click or
    // Escape; clicks inside the panel are swallowed so only the backdrop dismisses.
    def inspectOverlay(stack: Stack): Element =
      div(
        cls := "inspect-backdrop",
        onClick --> (_ => inspecting.set(None)),
        documentEvents(_.onKeyDown).filter(_.key == "Escape") --> (_ => inspecting.set(None)),
        div(
          cls := "inspect-panel",
          onClick --> (e => e.stopPropagation()),
          div(
            cls := "inspect-header",
            span(cls := "inspect-title", s"${stack.label} (${stack.cards.size})".trim),
            button(cls := "inspect-close", title := "Close", "✕", onClick --> (_ => inspecting.set(None))),
          ),
          div(cls := "inspect-grid", stack.cards.map(inspectCardView(stack, _))),
        ),
      )

    // The manual-effect prompt: a small, non-modal dialog living inside the world,
    // placed just to the right of the card whose arrival paused the cascade — in
    // board coordinates, so it rides the camera's pan and zoom. It tracks the card
    // for as long as it exists: should the player drag the card to another stack
    // mid-pause, the dialog follows it there. Non-modal on purpose — the board stays
    // live so the player can carry the effect out by hand; only "Done" resumes.
    def manualDialog(prompt: ManualPrompt): Element =
      val anchor = state.signal.map(_.stacks.find(_.cards.exists(_.id == prompt.card)).map(_.position)).distinct
      div(
        cls := "manual-dialog",
        // Anchor beside the triggering card while it's on the table. A prompt with no
        // such card — a button's own Manual, or a card that has since left — pins to a
        // fixed spot in the viewport instead (board coords undoing the current pan and
        // zoom), so it stays visible and dismissible rather than vanishing.
        styleAttr <-- anchor.combineWith(pan.signal).combineWith(zoom.signal).map {
          case (Some(p), _, _) => s"left:${p.x + cardWidth + 8}px;top:${p.y}px"
          case (None, pn, z)   => s"left:${((24 - pn.x) / z).toInt}px;top:${((96 - pn.y) / z).toInt}px"
        },
        div(cls := "manual-desc", prompt.description),
        button(cls := "btn btn-primary", "Done", onClick --> (_ => completeManual(prompt))),
      )

    // The stack-choice prompt: same anchored, camera-riding dialog as the manual one,
    // but it asks the player to click a stack rather than to act by hand. Cancel
    // abandons the rest of the cascade; there is no "Done" — picking a stack resumes.
    def choiceDialog(prompt: ChoicePrompt): Element =
      val anchor = state.signal.map(_.stacks.find(_.cards.exists(_.id == prompt.card)).map(_.position)).distinct
      val what = prompt.slot match
        case "from" => "deal from"
        case "to"   => "deal onto"
        case _      => "shuffle"
      div(
        cls := "manual-dialog choice-dialog",
        styleAttr <-- anchor.combineWith(pan.signal).combineWith(zoom.signal).map {
          case (Some(p), _, _) => s"left:${p.x + cardWidth + 8}px;top:${p.y}px"
          case (None, pn, z)   => s"left:${((24 - pn.x) / z).toInt}px;top:${((96 - pn.y) / z).toInt}px"
        },
        div(cls := "manual-desc", s"Click the stack to $what."),
        button(cls := "btn", "Cancel", onClick --> (_ => cancelChoice())),
      )

    // The card-choice prompt: the same anchored dialog, asking the player to click the
    // card to move. A pile shows only its top, so clicking one picks the top; a row
    // shows every card, so a specific one can be picked. Cancel abandons the cascade.
    def cardChoiceDialog(prompt: CardChoicePrompt): Element =
      val anchor = state.signal.map(_.stacks.find(_.cards.exists(_.id == prompt.card)).map(_.position)).distinct
      div(
        cls := "manual-dialog choice-dialog",
        styleAttr <-- anchor.combineWith(pan.signal).combineWith(zoom.signal).map {
          case (Some(p), _, _) => s"left:${p.x + cardWidth + 8}px;top:${p.y}px"
          case (None, pn, z)   => s"left:${((24 - pn.x) / z).toInt}px;top:${((96 - pn.y) / z).toInt}px"
        },
        div(cls := "manual-desc", "Click the card to move — the top of a pile, or any card in a row."),
        button(cls := "btn", "Cancel", onClick --> (_ => cancelCardChoice())),
      )

    // Snap every authored stack back to the position it started at. Loose stacks
    // split off mid-play have no authored home, so they're left where they lie.
    def restorePositions(): Unit =
      if !animating then
        state.update(s => definition.setup.stacks.foldLeft(s)((acc, sp) => Engine.moveStack(acc, sp.id, sp.position).getOrElse(acc)))

    // Throw away the in-progress table and re-deal from the authored setup. The
    // save observer below rewrites storage from the fresh state, so a restarted
    // game is itself the new ongoing game.
    def restartGame(): Unit =
      if !animating && dom.window.confirm("Restart this game from a fresh setup? Your current progress will be lost.") then
        manualPause.set(None)     // drop any pending prompt; its continuation is for the old table
        choicePause.set(None)     // likewise any pending stack choice — it belongs to the old table
        cardChoicePause.set(None) // and any pending card choice
        state.set(freshSetup())

    div(
      cls := "play-screen",
      // The card text size and the shared layout's dimensions/fill, exposed as CSS
      // variables the cards inherit. Lives on the root so they also reach the inspect
      // overlay's cards, which mount outside the board. The layout is fixed per game,
      // so only the live font size drives the signal.
      styleAttr <-- cardFont.signal.distinct.map(f => s"--card-font:${f}rem;${CardFace.vars(definition.layout)}"),
      // Mirror every settled table into storage so a reload resumes mid-game. The
      // catalog and rules don't change in play, so only the stacks and whose turn
      // it is are saved.
      state.signal.map(s => SavedGame(s.stacks, s.currentPlayer)).distinct --> (saved => GameStore.saveGame(definition.id, saved)),
      div(
        cls := "toolbar",
        display <-- toolbarVisible.signal.map(v => if v then "flex" else "none").distinct,
        button(cls := "btn", "← Library", onClick --> (_ => onBack())),
        span(cls := "toolbar-title", definition.name),
        button(
          cls   := "btn",
          title := "Move every stack back to its starting position",
          "⟲ Restore positions",
          onClick --> (_ => restorePositions()),
        ),
        button(
          cls   := "btn",
          title := "Discard this game and deal a fresh setup",
          "↻ Restart game",
          onClick --> (_ => restartGame()),
        ),
        // Card text size — independent of zoom. Bounded so the text can't vanish or
        // swamp the card; one notch per click.
        div(
          cls := "font-control",
          button(
            cls   := "btn",
            title := "Smaller card text",
            "A−",
            onClick --> (_ => cardFont.update(f => (f / fontStep).max(minFont))),
          ),
          span(cls := "font-control-value", child.text <-- cardFont.signal.map(f => s"${(f * 100).round}%").distinct),
          button(
            cls   := "btn",
            title := "Larger card text",
            "A+",
            onClick --> (_ => cardFont.update(f => (f * fontStep).min(maxFont))),
          ),
        ),
        // Collapses the bar to hand its height to the field. A natural last item in
        // the row so it matches the other buttons' size and alignment.
        button(
          cls   := "btn",
          title := "Hide the top bar",
          "▲ Hide bar",
          onClick --> (_ => toolbarVisible.set(false)),
        ),
      ),
      // The only affordance left once the bar is gone — a corner button to bring it back.
      button(
        cls   := "btn toolbar-restore",
        title := "Show the top bar",
        display <-- toolbarVisible.signal.map(v => if v then "none" else "block").distinct,
        "▼ Show bar",
        onClick --> (_ => toolbarVisible.set(true)),
      ),
      div(cls := "game", board),
      // The inspect overlay, when a lens is open. Driven by both the open stack and
      // the live table so the grid tracks pulls, and so it closes itself if the
      // stack empties out (a non-persistent pile ceasing to exist).
      child <-- inspecting.signal
        .combineWith(state.signal)
        .distinct
        .map: (open, s) =>
          open.flatMap(id => s.stacks.find(_.id == id)) match
            case Some(stack) if stack.cards.nonEmpty => inspectOverlay(stack)
            case _                                   => emptyNode,
    )

  private val cascadeStep = 26

  // Wheel zoom: multiplicative step per notch, bounded so the table can't vanish.
  private val zoomStep = 1.03
  private val minZoom  = 0.3
  private val maxZoom  = 3.0

  // Card text size (rem multiplier): one notch per A−/A+ click, bounded so the
  // text stays legible without overflowing the fixed-size card.
  private val fontStep = 1.1
  private val minFont  = 0.6
  private val maxFont  = 2.0

  // A cascade failure rendered as one plain sentence for the player. A `Same`/`Owned`
  // reference that named no stack reads as the owning player and the missing role;
  // the rest name the offending stack or card. (Player indices are 0-based internally,
  // shown 1-based.)
  private def engineErrorMessage(err: EngineError): String = err match
    case EngineError.UnknownStack(id)    => s"This action targets a stack that doesn't exist: ${id.value}."
    case EngineError.UnknownCard(id)     => s"This action targets a card that doesn't exist: ${id.value}."
    case EngineError.UnknownCardDef(id)  => s"This action targets a card type that doesn't exist: ${id.value}."
    case EngineError.EmptyStack(id)      => s"This action needs a card from an empty stack: ${id.value}."
    case EngineError.UnresolvedRole(role, player) =>
      s"""Player ${player + 1} has no "$role" stack for this action to target."""
    case EngineError.UnresolvedSame(role, owner) =>
      owner match
        case Some(p) => s"""Player ${p + 1} has no "$role" stack for this action to target."""
        case None    => s"""There is no shared "$role" stack for this action to target."""

  // Kept in step with the .stack.shuffling animation duration in engine.css.
  private val shuffleAnimMs = 450

  // How long a single played card's slide / flip takes. The flip value is mirrored
  // by the .card-flipping animation duration in engine.css.
  private val moveAnimMs = 320
  private val flipAnimMs = 320

  // A play touching this many cards or more (moves + flips) animates each step much
  // faster, so big deals, sweeps, and flip-everything migrations don't crawl through
  // the whole batch one slow step at a time.
  private val bulkMoveThreshold = 5
  private val bulkMoveAnimMs    = 120
  private val bulkFlipAnimMs    = 120
