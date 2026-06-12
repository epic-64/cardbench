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

  def view(): Element =
    val initial = Engine.setup(SampleGame.catalog, SampleGame.rulebook, SampleGame.setup, System.currentTimeMillis())
    val state   = Var(initial)
    val catalog = initial.catalog

    var dragCard: Option[CardDrag]   = None
    var dragStack: Option[StackDrag] = None
    var looseSeq                     = 0 // supplies fresh ids for stacks split off the board
    var animating                    = false // true while a play's step script is running

    // Which stack is mid-shuffle: drives the shake animation, cleared on a timer.
    val shuffling = Var(Option.empty[StackId])

    lazy val board: HtmlElement = div(
      cls := "board",
      onDragOver --> (e => e.preventDefault()),
      onDrop --> (e => onBoardDrop(e)),
      children <-- state.signal.map(_.stacks).distinct.map(_.map(stackView)),
    )

    // Where the pointer sits on the board right now, in board pixels.
    def boardPoint(clientX: Double, clientY: Double): Position =
      val rect = board.ref.getBoundingClientRect()
      Position((clientX - rect.left).toInt, (clientY - rect.top).toInt)

    // A card dropped on empty board space splits into a new stack at the point
    // under the cursor. Drops onto a stack are handled by the stack itself.
    def onBoardDrop(e: dom.DragEvent): Unit =
      if animating then ()
      else
        e.preventDefault()
        val p = boardPoint(e.clientX, e.clientY)
        dragCard.foreach: c =>
          looseSeq += 1
          val newId = StackId(s"loose#$looseSeq")
          val to    = Position(clamp(p.x - c.offX), clamp(p.y - c.offY))
          state.update(s => Engine.extractCard(s, c.id, newId, to).getOrElse(s))
        dragCard = None

    def stackView(stack: Stack): Element =
      val body = stack.layout match
        case Layout.Pile => topView(stack)
        case Layout.Row  => rowView(stack)
      // Controls (shuffle + move handle) only make sense for a multi-card pile; a
      // single-card stack you nudge by its lone card, and a row zone stays put.
      val controls =
        if stack.layout == Layout.Pile && stack.cards.size > 1 then sideControls(stack) else emptyNode
      div(
        cls       := "stack",
        cls("stack-play") := (stack.id == SampleGame.playZone),
        cls("stack-row") := (stack.layout == Layout.Row),
        cls("shuffling") <-- shuffling.signal.map(_.contains(stack.id)).distinct,
        styleAttr := s"left:${stack.position.x}px;top:${stack.position.y}px",
        onDragOver --> (e => e.preventDefault()),
        onDrop --> (e => onStackDrop(e, stack)),
        labelView(stack),
        body,
        controls,
      )

    // The count badge is only meaningful for a real pile; a lone card shows just
    // its label (and a loose, unlabelled card shows nothing).
    def labelView(stack: Stack): Node =
      val count = stack.cards.size
      if count > 1 then div(cls := "stack-label", s"${stack.label} ($count)".trim)
      else if stack.label.nonEmpty then div(cls := "stack-label", stack.label)
      else emptyNode

    // A vertical control column hugging the stack's bottom-right: a small
    // shuffle button above the drag-to-move handle.
    def sideControls(stack: Stack): Element =
      div(
        cls := "stack-side",
        button(
          cls   := "stack-shuffle",
          title := "Shuffle this stack",
          "⇄",
          onClick --> { _ =>
            // Mark the stack first so the re-rendered element mounts with the
            // animation class already on; a timer clears it once the shake ends.
            if !animating then
              shuffling.set(Some(stack.id))
              dom.window.setTimeout(() => shuffling.set(None), shuffleAnimMs)
              state.update(s => Engine.shuffle(s, stack.id, System.currentTimeMillis()).getOrElse(s))
          },
        ),
        moveHandle(stack),
      )

    // ── animated play ──────────────────────────────────────────────────────
    // A play resolves as a script of atomic steps (Engine.playSteps); we run it
    // one step at a time so each move/flip can animate, and ignore drops while a
    // script is in flight (see `animating`).

    def cardEl(card: CardId): Option[dom.html.Element] =
      Option(board.ref.querySelector(s"[data-card-id=\"${card.value}\"]"))
        .map(_.asInstanceOf[dom.html.Element])

    // FLIP: the card has just re-rendered at its new home; slide it visually from
    // `from` (where it sat a moment ago) to rest, so the jump reads as a glide.
    def slide(card: CardId, from: Option[dom.DOMRect]): Unit =
      (cardEl(card), from) match
        case (Some(el), Some(f)) =>
          val now = el.getBoundingClientRect()
          el.style.zIndex = "10"
          el.style.transition = "none"
          el.style.transform = s"translate(${f.left - now.left}px, ${f.top - now.top}px)"
          el.getBoundingClientRect() // force the start offset to register before transitioning
          el.style.transition = s"transform ${moveAnimMs}ms ease"
          el.style.transform = "translate(0px, 0px)"
        case _ => ()

    def flipReveal(card: CardId): Unit =
      cardEl(card).foreach(_.classList.add("card-flipping"))

    def animateStep(step: Step, done: () => Unit): Unit =
      step match
        case Step.Move(card, to) =>
          val from = cardEl(card).map(_.getBoundingClientRect())
          state.update(s => Engine.move(s, card, to).getOrElse(s))
          slide(card, from)
          dom.window.setTimeout(() => done(), moveAnimMs)
        case Step.Flip(card) =>
          state.update(s => Engine.flip(s, card).getOrElse(s))
          flipReveal(card)
          dom.window.setTimeout(() => done(), flipAnimMs)

    def runScript(steps: List[Step]): Unit =
      def loop(remaining: List[Step]): Unit =
        remaining match
          case Nil          => animating = false
          case step :: rest => animateStep(step, () => loop(rest))
      animating = true
      loop(steps)

    // Build the play's script: the card sliding into the play zone, then its
    // effects resolving on top (one of which may move it on to its post-play
    // home). A play with nothing to do (Left) is simply dropped.
    def playAnimated(card: CardId): Unit =
      Engine.playSteps(state.now(), card, SampleGame.playZone).foreach(runScript)

    // A card dropped onto a *different* stack merges on top. Dropped onto its own
    // single-card stack it just repositions — so a lone card can be nudged
    // anywhere, not only by dragging it clear of the stack's own bounds. Dropped
    // onto the play zone (and not already in it) it's played, animating.
    def onStackDrop(e: dom.DragEvent, stack: Stack): Unit =
      if animating then ()
      else
        dragCard.foreach: c =>
          e.preventDefault()
          e.stopPropagation()
          val selfLone    = stack.cards.size == 1 && stack.cards.exists(_.id == c.id)
          val playingHere = stack.id == SampleGame.playZone && !stack.cards.exists(_.id == c.id)
          val p           = boardPoint(e.clientX, e.clientY)
          dragCard = None
          if playingHere then playAnimated(c.id)
          else
            val verb: GameState => Either[EngineError, GameState] =
              if selfLone then Engine.moveStack(_, stack.id, Position(clamp(p.x - c.offX), clamp(p.y - c.offY)))
              else Engine.move(_, c.id, stack.id)
            state.update(s => verb(s).getOrElse(s))

    // Pointer-driven, so movement is pixel-exact and starts immediately. We move
    // the live element directly during the drag (no re-render churn that would
    // drop the pointer capture) and commit the final position on release.
    def moveHandle(stack: Stack): Element =
      div(
        cls   := "stack-handle",
        title := "Drag to move this stack",
        "⠿",
        onPointerDown --> { e =>
          if !animating then
            e.preventDefault()
            val handle  = e.currentTarget.asInstanceOf[dom.Element]
            val stackEl = handle.parentNode.parentNode.asInstanceOf[dom.html.Element]
            val grab    = boardPoint(e.clientX, e.clientY)
            // setPointerCapture keeps move/up events on the handle once the cursor
            // leaves it; it's missing from this scala-js-dom facade, so call it raw.
            handle.asInstanceOf[js.Dynamic].setPointerCapture(e.pointerId)
            dragStack = Some(StackDrag(stackEl, grab.x - stack.position.x, grab.y - stack.position.y))
        },
        onPointerMove --> { e =>
          dragStack.foreach: d =>
            val p = boardPoint(e.clientX, e.clientY)
            d.el.style.left = s"${clamp(p.x - d.grabX)}px"
            d.el.style.top = s"${clamp(p.y - d.grabY)}px"
        },
        onPointerUp --> { e =>
          dragStack.foreach: d =>
            val p = boardPoint(e.clientX, e.clientY)
            e.currentTarget.asInstanceOf[js.Dynamic].releasePointerCapture(e.pointerId)
            dragStack = None
            state.update(s => Engine.moveStack(s, stack.id, Position(clamp(p.x - d.grabX), clamp(p.y - d.grabY))).getOrElse(s))
        },
        onPointerCancel --> (_ => dragStack = None),
      )

    // One card instance: draggable, click-to-flip, tagged with its id so the
    // animator can find it. `depth` adds the layered-deck look; `badge` overlays
    // a mark (e.g. the shuffle hint). Shared by the pile and row layouts.
    def renderCard(stack: Stack, card: CardInstance, depth: Seq[String], badge: Node): Element =
      val startDrag = onDragStart --> { e =>
        val el = e.currentTarget.asInstanceOf[dom.html.Element]
        val r  = el.getBoundingClientRect()
        dragCard = Some(CardDrag(card.id, (e.clientX - r.left).toInt, (e.clientY - r.top).toInt))
        e.dataTransfer.setData("text/plain", card.id.value)
        // Hide the source after the browser has captured the drag ghost (it
        // grabs the image synchronously at dragstart), so a lone card shows
        // only the ghost and doesn't appear to split in two.
        dom.window.setTimeout(() => el.classList.add("card-dragging"), 0)
      }
      val endDrag = onDragEnd --> { e =>
        e.currentTarget.asInstanceOf[dom.html.Element].classList.remove("card-dragging")
        dragCard = None
      }
      val flip = onClick --> (_ => if !animating then state.update(s => Engine.flip(s, card.id).getOrElse(s)))
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
          val d = catalog.get(card.defId)
          div(
            cls       := Seq("card", "card-front") ++ depth,
            draggable := true,
            dataAttr("card-id") := card.id.value,
            styleAttr := d.map(c => s"border-top:4px solid ${c.color}").getOrElse(""),
            div(cls := "card-title", d.map(_.title).getOrElse(card.defId.value)),
            div(cls := "card-desc", d.map(_.description).getOrElse("")),
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

    div(
      cls := "game",
      board,
    )

  private def clamp(n: Int): Int = math.max(0, n)

  // Kept in step with the .stack.shuffling animation duration in engine.css.
  private val shuffleAnimMs = 450

  // How long a single played card's slide / flip takes. The flip value is mirrored
  // by the .card-flipping animation duration in engine.css.
  private val moveAnimMs = 320
  private val flipAnimMs = 320
