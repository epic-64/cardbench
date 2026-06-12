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

    def setPosition(id: StackId, pos: Position): Unit =
      draft.update: d =>
        d.copy(setup = GameSetup(d.setup.stacks.map(s => if s.id == id then s.copy(position = pos) else s)))

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

    def stackBox(spec: StackSpec): Element =
      val cards = spec.contents.map(_.count).sum
      div(
        cls       := "tune-stack",
        styleAttr := s"left:${spec.position.x}px;top:${spec.position.y}px",
        div(cls := "tune-stack-label", if spec.label.nonEmpty then spec.label else spec.id.value),
        div(cls := "tune-stack-meta", s"$cards cards · ${spec.layout.toString.toLowerCase}"),
        div(cls := "tune-stack-pos", s"${spec.position.x}, ${spec.position.y}"),
        onPointerDown --> (e => startDrag(e)),
        onPointerMove --> (e => moveDrag(e)),
        onPointerUp --> (e => endDrag(e, spec.id)),
        onPointerCancel --> (_ => drag = None),
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
        Option(d.el.querySelector(".tune-stack-pos")).foreach(_.textContent = s"$x, $y")

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

  // The drag surface stretches to hold the furthest stack, with a screenful of
  // slack beyond so a stack can be pulled out past the current edge.
  private def canvasExtent(d: GameDefinition): (Int, Int) =
    val xs = d.setup.stacks.map(_.position.x)
    val ys = d.setup.stacks.map(_.position.y)
    (math.max(1200, (if xs.isEmpty then 0 else xs.max) + 360), math.max(700, (if ys.isEmpty then 0 else ys.max) + 360))
