package client

import com.raquo.laminar.api.L.*
import engine.*
import org.scalajs.dom
import scala.scalajs.js

/** Lay out a game's stacks by dragging them on a grid, the visual counterpart to
  * the editor's numeric X/Y fields. A `Var[GameDefinition]` is the working copy;
  * each drag rewrites one stack's `position`. Save persists; cancel discards.
  *
  * Dragging follows the same pattern as the play board: the live element is moved
  * directly during the drag (so a re-render can't drop the pointer capture) and
  * the snapped position is committed to the draft on release. Positions snap to
  * the grid unless snapping is turned off, and never go negative.
  */
object FineTuneView:

  private final case class Drag(el: dom.html.Element, grabX: Double, grabY: Double)

  def view(
    initial: GameDefinition,
    onSave: GameDefinition => Unit,
    onCancel: () => Unit,
  ): Element =
    val draft    = Var(initial)
    val gridSize = Var(20)
    val snap     = Var(true)
    var drag     = Option.empty[Drag]
    val catalog  = initial.catalog.cards.map(c => c.id -> c).toMap

    def setPosition(id: StackId, pos: Position): Unit =
      draft.update: d =>
        d.copy(setup = d.setup.copy(stacks = d.setup.stacks.map(s => if s.id == id then s.copy(position = pos) else s)))

    // Round to the nearest grid line (when snapping is on), never below zero.
    def snapClamp(v: Double): Int =
      val g       = gridSize.now()
      val snapped = if snap.now() then math.round(v / g).toInt * g else v.round.toInt
      math.max(0, snapped)

    lazy val canvas: HtmlElement = div(
      cls := "tune-canvas",
      // The grid spacing follows the snap grid; the canvas grows to hold the
      // furthest stack plus room to drag past it.
      styleAttr <-- gridSize.signal.combineWith(draft.signal).map: (g, d) =>
        val (w, h) = canvasExtent(d)
        s"--grid:${g}px;width:${w}px;height:${h}px",
      children <-- draft.signal.map(_.setup.stacks).distinct.map(_.map(stackBox)),
    )

    // Where the pointer sits in canvas pixels — `getBoundingClientRect` already
    // folds in any scroll, so this stays correct on a scrolled board.
    def canvasPoint(clientX: Double, clientY: Double): (Double, Double) =
      val r = canvas.ref.getBoundingClientRect()
      (clientX - r.left, clientY - r.top)

    // A stack drawn with the play board's own markup, so its footprint here is
    // exactly what it will occupy in play: a pile shows one card (layered when it
    // holds more), a row spreads every card side by side. The whole stack is the
    // drag handle; a corner badge reads out its live position.
    def stackBox(spec: StackSpec): Element =
      div(
        cls := "stack tune-draggable",
        cls("stack-row") := (spec.layout == Layout.Row),
        styleAttr := s"left:${spec.position.x}px;top:${spec.position.y}px",
        stackLabel(spec),
        stackBody(spec),
        div(cls := "tune-pos", s"${spec.position.x}, ${spec.position.y}"),
        onPointerDown --> (e => startDrag(e)),
        onPointerMove --> (e => moveDrag(e)),
        onPointerUp --> (e => endDrag(e, spec.id)),
        onPointerCancel --> (_ => drag = None),
      )

    def stackLabel(spec: StackSpec): Node =
      val n = cardCount(spec)
      if n > 1 then div(cls := "stack-label", s"${spec.label} ($n)".trim)
      else if spec.label.nonEmpty then div(cls := "stack-label", spec.label)
      else emptyNode

    def stackBody(spec: StackSpec): Element =
      val cards = expand(spec)
      spec.layout match
        case Layout.Pile =>
          cards.headOption match
            case None      => div(cls := "card card-empty", "—")
            case Some(top) => cardFace(top, spec.facing, stacked = cards.size > 1)
        case Layout.Row =>
          if cards.isEmpty then div(cls := "card card-empty", "—")
          else div(cls := "zone-row", cards.map(cardFace(_, spec.facing, stacked = false)))

    // A face, dimensionally identical to a real card: a back when face-down, else
    // a colour-topped front with its title, so stacks stay tellable apart.
    def cardFace(defId: CardDefId, facing: Facing, stacked: Boolean): Element =
      val depth = if stacked then Seq("card-stacked") else Nil
      facing match
        case Facing.Down =>
          div(cls := (Seq("card", "card-back") ++ depth))
        case Facing.Up =>
          val d = catalog.get(defId)
          div(
            cls       := (Seq("card", "card-front") ++ depth),
            styleAttr := d.map(c => s"border-top:4px solid ${c.color}").getOrElse(""),
            div(cls := "card-title", d.map(_.title).getOrElse(defId.value)),
          )

    def startDrag(e: dom.PointerEvent): Unit =
      e.preventDefault()
      val el = e.currentTarget.asInstanceOf[dom.html.Element]
      val b  = el.getBoundingClientRect()
      el.asInstanceOf[js.Dynamic].setPointerCapture(e.pointerId)
      el.classList.add("tune-dragging")
      drag = Some(Drag(el, e.clientX - b.left, e.clientY - b.top))

    def moveDrag(e: dom.PointerEvent): Unit =
      drag.foreach: d =>
        val (px, py) = canvasPoint(e.clientX, e.clientY)
        val x        = snapClamp(px - d.grabX)
        val y        = snapClamp(py - d.grabY)
        d.el.style.left = s"${x}px"
        d.el.style.top = s"${y}px"
        // Keep the readout honest while the stack is still in flight.
        Option(d.el.querySelector(".tune-pos")).foreach(_.textContent = s"$x, $y")

    def endDrag(e: dom.PointerEvent, id: StackId): Unit =
      drag.foreach: d =>
        val (px, py) = canvasPoint(e.clientX, e.clientY)
        e.currentTarget.asInstanceOf[js.Dynamic].releasePointerCapture(e.pointerId)
        d.el.classList.remove("tune-dragging")
        drag = None
        setPosition(id, Position(snapClamp(px - d.grabX), snapClamp(py - d.grabY)))

    div(
      cls := "fine-tune",
      div(
        cls := "toolbar",
        button(cls := "btn", "← Cancel", onClick --> (_ => onCancel())),
        span(cls := "toolbar-title", s"Fine-tune layout — ${initial.name}"),
        gridSizeSelect(gridSize),
        snapToggle(snap),
        button(cls := "btn btn-primary", "Save", onClick --> (_ => onSave(draft.now()))),
      ),
      div(
        cls := "tune-board",
        child <-- draft.signal.map(_.setup.stacks.isEmpty).distinct.map {
          case true  => div(cls := "tune-empty", "This game has no stacks to position yet.")
          case false => canvas
        },
      ),
    )

  private def gridSizeSelect(gridSize: Var[Int]): Element =
    select(
      cls := "field-input",
      onChange.mapToValue --> (v => v.toIntOption.foreach(gridSize.set)),
      List(10, 20, 40).map(g => option(value := g.toString, selected := (g == gridSize.now()), s"${g}px grid")),
    )

  private def snapToggle(snap: Var[Boolean]): Element =
    label(
      cls := "field field-check",
      input(typ := "checkbox", defaultChecked := true, onInput --> (e => snap.set(e.target.asInstanceOf[dom.html.Input].checked))),
      span(cls := "field-label", "Snap"),
    )

  // The drag surface stretches to hold each stack's far edge — position plus its
  // real footprint — with a screenful of slack beyond so a stack can be pulled
  // out past the current edge.
  private def canvasExtent(d: GameDefinition): (Int, Int) =
    val edges = d.setup.stacks.map: s =>
      val (w, h) = footprint(s)
      (s.position.x + w, s.position.y + h)
    val right  = if edges.isEmpty then 0 else edges.map(_._1).max
    val bottom = if edges.isEmpty then 0 else edges.map(_._2).max
    (math.max(1200, right + 200), math.max(700, bottom + 200))

  // A stack's pixel footprint, mirroring the play board: card dimensions match the
  // --card-w / --card-h / row gap in engine.css, plus room for the label above.
  private val cardW   = 130
  private val cardH   = 180
  private val rowGap  = 8
  private val labelH  = 28
  private def footprint(spec: StackSpec): (Int, Int) =
    val n = math.max(1, cardCount(spec))
    val w = spec.layout match
      case Layout.Row  => n * cardW + (n - 1) * rowGap
      case Layout.Pile => cardW + 6 // the layered-deck shadow offset
    (w, labelH + cardH)

  private def expand(spec: StackSpec): List[CardDefId] =
    spec.contents.flatMap(s => List.fill(s.count)(s.card))

  private def cardCount(spec: StackSpec): Int =
    spec.contents.map(_.count).sum
