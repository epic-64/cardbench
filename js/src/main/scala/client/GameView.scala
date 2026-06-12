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

  def view(definition: GameDefinition, onBack: () => Unit): Element =
    val initial = Engine.setup(definition.catalog, definition.rulebook, definition.setup, System.currentTimeMillis())
    val state   = Var(initial)
    val catalog = initial.catalog

    var dragCard: Option[CardDrag]   = None
    var dragStack: Option[StackDrag] = None
    var looseSeq                     = 0 // supplies fresh ids for stacks split off the board
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

    // Camera over the free-form table: a pan offset (screen px) and a zoom, so a
    // crowded board can be scrolled and scaled. The stacks live inside `world`,
    // which carries the matching transform; the board itself is the fixed viewport.
    val pan  = Var(Position(0, 0))
    val zoom = Var(1.0)
    var panning: Option[(Double, Double, Position)] = None // pointer-down client x/y + pan at grab

    lazy val world: HtmlElement = div(
      cls := "world",
      styleAttr <-- pan.signal
        .combineWith(zoom.signal)
        .distinct
        .map((p, z) => s"transform:translate(${p.x}px,${p.y}px) scale($z)"),
      children <-- state.signal.map(_.stacks).distinct.map(_.map(stackView)),
    )

    lazy val board: HtmlElement = div(
      cls := "board",
      onDragOver --> (e => onBoardDragOver(e)),
      onDrop --> (e => onBoardDrop(e)),
      onWheel --> (e => onZoom(e)),
      onPointerDown --> (e => onPanStart(e)),
      onPointerMove --> (e => onPanMove(e)),
      onPointerUp --> (e => onPanEnd(e)),
      onPointerCancel --> (_ => panning = None),
      world,
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
      // Every stack gets a drag-to-move handle — even empty, single-card, or row
      // zones; the shuffle button rides along only where it makes sense.
      val controls = sideControls(stack)
      div(
        cls       := "stack",
        cls("stack-row") := (stack.layout == Layout.Row),
        cls("shuffling") <-- shuffling.signal.map(_.contains(stack.id)).distinct,
        styleAttr := s"left:${stack.position.x}px;top:${stack.position.y}px",
        onDragOver --> (e => e.preventDefault()),
        onDrop --> (e => onStackDrop(e, stack)),
        labelView(stack),
        body,
        // Authored buttons sit below the card, one per row, so they never cover
        // its title or text.
        stackButtons(stack),
        controls,
      )

    // The count badge is only meaningful for a real pile; a lone card shows just
    // its label (and a loose, unlabelled card shows nothing).
    def labelView(stack: Stack): Node =
      val count = stack.cards.size
      if count > 1 then div(cls := "stack-label", s"${stack.label} ($count)".trim)
      else if stack.label.nonEmpty then div(cls := "stack-label", stack.label)
      else emptyNode

    // A vertical control column hugging the stack's bottom-right: the drag-to-move
    // handle on every stack, with a shuffle button above it only where shuffling
    // makes sense — a pile of more than one card.
    def sideControls(stack: Stack): Element =
      val canShuffle = stack.layout == Layout.Pile && stack.cards.size > 1
      div(
        cls := "stack-side",
        if canShuffle then shuffleButton(stack) else emptyNode,
        moveHandle(stack),
      )

    def shuffleButton(stack: Stack): Element =
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
      )

    // Authored shortcuts bound to this stack: each clicks through to a deal
    // (drawing into the stack, or dealing out of it) that animates and fires rules
    // just like a drag would. None render on loose stacks split off mid-play.
    def stackButtons(stack: Stack): Node =
      definition.setup.buttons.filter(_.stackId == stack.id) match
        case Nil     => emptyNode
        case buttons => div(cls := "stack-buttons", buttons.map(buttonControl(stack, _)))

    def buttonControl(stack: Stack, b: StackButton): Element =
      button(
        cls := "stack-button",
        b.label,
        onClick --> (_ => runButton(stack, b)),
      )

    // A button deals between its own stack and the action's other stack: DealFrom
    // draws into this stack, DealTo deals out of it. Ignored while a script runs;
    // a deal from an empty source simply resolves to nothing.
    def runButton(stack: Stack, b: StackButton): Unit =
      if !animating then
        val (from, to, count) = b.action match
          case ButtonAction.DealFrom(src, c) => (src, stack.id, c)
          case ButtonAction.DealTo(dst, c)   => (stack.id, dst, c)
        Engine.dealSteps(state.now(), from, to, count).foreach(runScript(_))

    // ── animated drops ─────────────────────────────────────────────────────
    // A drop resolves as a script of atomic steps (Engine.dropSteps); we run it
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
          // `f`/`now` are screen px, but the transform lives inside the zoomed
          // world, which scales it — so divide by zoom to glide the true distance.
          val z = zoom.now()
          el.style.zIndex = "10"
          el.style.transition = "none"
          el.style.transform = s"translate(${(f.left - now.left) / z}px, ${(f.top - now.top) / z}px)"
          el.getBoundingClientRect() // force the start offset to register before transitioning
          el.style.transition = s"transform ${moveAnimMs}ms ease"
          el.style.transform = "translate(0px, 0px)"
          // The lift is only for the glide; clear it afterwards or the card keeps
          // a z-index of 10 and covers the stack's controls (move handle, shuffle).
          dom.window.setTimeout(
            () =>
              el.style.zIndex = ""
              el.style.transition = ""
              el.style.transform = "",
            moveAnimMs,
          )
        case _ => ()

    def flipReveal(card: CardId): Unit =
      cardEl(card).foreach(_.classList.add("card-flipping"))

    // `fromOverride` lets a drop start the dropped card's glide from where the
    // user released it (the floating ghost) instead of its origin — otherwise the
    // card snaps back to its source and slides forward, a move the drag already made.
    def animateStep(step: Step, fromOverride: Option[dom.DOMRect], done: () => Unit): Unit =
      step match
        case Step.Move(card, to) =>
          val from = fromOverride.orElse(cardEl(card).map(_.getBoundingClientRect()))
          state.update(s => Engine.move(s, card, to).getOrElse(s))
          slide(card, from)
          dom.window.setTimeout(() => done(), moveAnimMs)
        case Step.Flip(card) =>
          state.update(s => Engine.flip(s, card).getOrElse(s))
          flipReveal(card)
          dom.window.setTimeout(() => done(), flipAnimMs)

    // `firstFrom` overrides only the first step's glide origin (the dropped card);
    // every reaction step that follows glides from its own real position.
    def runScript(steps: List[Step], firstFrom: Option[dom.DOMRect] = None): Unit =
      def loop(remaining: List[Step], fromOverride: Option[dom.DOMRect]): Unit =
        remaining match
          case Nil          => animating = false
          case step :: rest => animateStep(step, fromOverride, () => loop(rest, None))
      animating = true
      loop(steps, firstFrom)

    // Drop a card onto a stack: relocate it and play out whatever reactions the
    // rules fire, as an animated step script. The card just merging on top with no
    // rule to react is simply the one-step case. A drop that resolves to nothing
    // (Left) is ignored.
    def dropAnimated(card: CardId, onto: StackId, from: Option[dom.DOMRect]): Unit =
      Engine.dropSteps(state.now(), card, onto).foreach(runScript(_, from))

    // A card dropped onto a stack relocates onto it (and the rules react). Dropped
    // onto its own single-card stack it just repositions instead — so a lone card
    // can be nudged anywhere, not only by dragging it clear of the stack's bounds.
    def onStackDrop(e: dom.DragEvent, stack: Stack): Unit =
      // Grab the floating ghost's screen position before tearing it down, so the
      // dropped card can glide from where it was released rather than its origin.
      val released = dragGhost.map((g, _, _) => g.getBoundingClientRect())
      clearDragGhost()
      if animating then ()
      else
        dragCard.foreach: c =>
          e.preventDefault()
          e.stopPropagation()
          val selfLone = stack.cards.size == 1 && stack.cards.exists(_.id == c.id)
          val p        = boardPoint(e.clientX, e.clientY)
          dragCard = None
          if selfLone then
            state.update(s => Engine.moveStack(s, stack.id, Position(clamp(p.x - c.offX), clamp(p.y - c.offY))).getOrElse(s))
          else dropAnimated(c.id, stack.id, released)

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

    // Snap every authored stack back to the position it started at. Loose stacks
    // split off mid-play have no authored home, so they're left where they lie.
    def restorePositions(): Unit =
      if !animating then
        state.update(s => definition.setup.stacks.foldLeft(s)((acc, sp) => Engine.moveStack(acc, sp.id, sp.position).getOrElse(acc)))

    div(
      cls := "play-screen",
      div(
        cls := "toolbar",
        button(cls := "btn", "← Library", onClick --> (_ => onBack())),
        span(cls := "toolbar-title", definition.name),
        button(
          cls   := "btn",
          title := "Move every stack back to its starting position",
          "⟲ Restore positions",
          onClick --> (_ => restorePositions()),
        ),
      ),
      div(cls := "game", board),
    )

  private def clamp(n: Int): Int = math.max(0, n)

  // Wheel zoom: multiplicative step per notch, bounded so the table can't vanish.
  private val zoomStep = 1.1
  private val minZoom  = 0.3
  private val maxZoom  = 3.0

  // Kept in step with the .stack.shuffling animation duration in engine.css.
  private val shuffleAnimMs = 450

  // How long a single played card's slide / flip takes. The flip value is mirrored
  // by the .card-flipping animation duration in engine.css.
  private val moveAnimMs = 320
  private val flipAnimMs = 320
