package client

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import engine.*

/** Throwaway playtest shell: one `Var[GameState]`, every verb an `update`.
  *
  * The board is free-form. Drag a card to split it off onto empty space (a new
  * stack) or drop it on another stack to combine. Drag a stack's ⠿ handle to
  * move the whole pile. Click a card to flip it.
  */
object GameView:

  // What the user is currently dragging, plus the grab offset within the dragged
  // element so the drop lands without jumping. JS is single-threaded, so a plain
  // mutable holder is safe here.
  private enum Drag:
    case OfCard(id: CardId, offX: Int, offY: Int)
    case OfStack(id: StackId, offX: Int, offY: Int)

  def view(): Element =
    val initial = Engine.setup(SampleGame.catalog, SampleGame.setup)
    val state   = Var(initial)
    val catalog = initial.catalog

    var drag: Option[Drag] = None
    var looseSeq           = 0 // supplies fresh ids for stacks split off the board

    lazy val board: HtmlElement = div(
      cls := "board",
      onDragOver --> (e => e.preventDefault()),
      onDrop --> (e => onBoardDrop(e)),
      children <-- state.signal.map(_.stacks).distinct.map(_.map(stackView)),
    )

    // A card dropped on empty board space splits into a new stack; a dragged
    // stack handle repositions its stack. Drops onto a stack are handled there.
    def onBoardDrop(e: dom.DragEvent): Unit =
      e.preventDefault()
      val rect = board.ref.getBoundingClientRect()
      val x    = (e.clientX - rect.left).toInt
      val y    = (e.clientY - rect.top).toInt
      drag.foreach:
        case Drag.OfStack(id, offX, offY) =>
          state.update(s => Engine.moveStack(s, id, Position(clamp(x - offX), clamp(y - offY))).getOrElse(s))
        case Drag.OfCard(id, offX, offY) =>
          looseSeq += 1
          val newId = StackId(s"loose#$looseSeq")
          state.update(s => Engine.extractCard(s, id, newId, Position(clamp(x - offX), clamp(y - offY))).getOrElse(s))
      drag = None

    def stackView(stack: Stack): Element =
      div(
        cls       := "stack",
        styleAttr := s"left:${stack.position.x}px;top:${stack.position.y}px",
        onDragOver --> (e => e.preventDefault()),
        onDrop --> (e => onStackDrop(e, stack)),
        div(cls := "stack-label", s"${stack.label} (${stack.cards.size})".trim),
        topView(stack),
        sideControls(stack),
      )

    // A vertical control column hugging the stack's bottom-right: a small
    // shuffle button above the drag-to-move handle.
    def sideControls(stack: Stack): Element =
      div(
        cls := "stack-side",
        button(
          cls   := "stack-shuffle",
          title := "Shuffle this stack",
          "⇄",
          onClick --> (_ => state.update(s => Engine.shuffle(s, stack.id, System.currentTimeMillis()).getOrElse(s))),
        ),
        moveHandle(stack),
      )

    // Dropping a card onto a stack merges it on top. Stack drags fall through to
    // the board (no stopPropagation) so the board can reposition them.
    def onStackDrop(e: dom.DragEvent, stack: Stack): Unit =
      drag match
        case Some(Drag.OfCard(cardId, _, _)) =>
          e.preventDefault()
          e.stopPropagation()
          state.update(s => Engine.move(s, cardId, stack.id).getOrElse(s))
          drag = None
        case _ => ()

    def moveHandle(stack: Stack): Element =
      div(
        cls       := "stack-handle",
        draggable := true,
        title     := "Drag to move this stack",
        "⠿",
        onDragStart --> { e =>
          // Offset of the pointer within the stack, derived from the board rect
          // and the stack's known position (no DOM traversal needed).
          val b    = board.ref.getBoundingClientRect()
          val offX = (e.clientX - b.left - stack.position.x).toInt
          val offY = (e.clientY - b.top - stack.position.y).toInt
          drag = Some(Drag.OfStack(stack.id, offX, offY))
          // Drag the ghost of the whole stack, not just the handle. The handle
          // is nested handle → .stack-side → .stack.
          val stackEl = e.currentTarget.asInstanceOf[dom.Node].parentNode.parentNode.asInstanceOf[dom.Element]
          e.dataTransfer.setDragImage(stackEl, offX, offY)
          e.dataTransfer.setData("text/plain", stack.id.value)
        },
        onDragEnd --> (_ => drag = None),
      )

    def topView(stack: Stack): Element =
      stack.cards.headOption match
        case None => div(cls := "card card-empty", "—")
        case Some(card) =>
          val startDrag = onDragStart --> { e =>
            val r = e.currentTarget.asInstanceOf[dom.Element].getBoundingClientRect()
            drag = Some(Drag.OfCard(card.id, (e.clientX - r.left).toInt, (e.clientY - r.top).toInt))
            e.dataTransfer.setData("text/plain", card.id.value)
          }
          val endDrag = onDragEnd --> (_ => drag = None)
          val flip    = onClick --> (_ => state.update(s => Engine.flip(s, card.id).getOrElse(s)))
          card.facing match
            case Facing.Down =>
              div(cls := "card card-back", draggable := true, startDrag, endDrag, flip)
            case Facing.Up =>
              val d = catalog.get(card.defId)
              div(
                cls       := "card card-front",
                draggable := true,
                styleAttr := d.map(c => s"border-top:4px solid ${c.color}").getOrElse(""),
                div(cls := "card-title", d.map(_.title).getOrElse(card.defId.value)),
                div(cls := "card-desc", d.map(_.description).getOrElse("")),
                startDrag,
                endDrag,
                flip,
              )

    div(
      cls := "game",
      h1("Card Engine — Playtest Table"),
      p(
        cls := "game-hint",
        "Drag a card onto empty space to split it off, or onto another stack to combine. " +
          "Drag the ⠿ handle to move a whole stack. Click a card to flip it.",
      ),
      board,
    )

  private def clamp(n: Int): Int = math.max(0, n)
