package client

import com.raquo.laminar.api.L.*
import engine.*

/** Renders a card's front from the shared [[CardLayout]], so the playtest board,
  * the inspect overlay, and the design-tab preview all draw cards the same way.
  *
  * A layout's dimensions, fill, and corner styling are global, so they ride on CSS
  * custom properties set once on a container ([[vars]]); the per-card accent, text,
  * and corner content fill the card body ([[boxes]]). Callers own the card root
  * element (its classes, drag handlers, data attributes) and just splice in the
  * style and children these return.
  */
object CardFace:

  /** CSS custom properties carrying a layout's base rectangle — size and
    * background — plus its shared corner styling onto a container, so every `.card`
    * inside takes the authored dimensions, fill, and corner look. Set on the play
    * screen and on the editor's preview.
    */
  def vars(layout: CardLayout): String =
    val c = layout.corner
    s"--card-w:${layout.width}px;--card-h:${layout.height}px;--card-bg:${layout.background};" +
      s"--corner-radius:${cornerRadius(c.shape)};--corner-font:${c.font}"

  /** The accent rule for a card front: a top border in the card's own colour, laid
    * over the layout's shared background, and that same colour exposed as the corner
    * slots' fill (`--corner-bg`). Applied to the card root.
    */
  def accent(color: String): String =
    s"border-top:4px solid $color;--corner-bg:$color"

  /** The body of a card front: the title and description, which flow to fill the
    * base rectangle, plus any non-empty corner slots. Spliced into the card root
    * alongside its drag handlers.
    */
  def boxes(title: String, description: String, corners: CardCorners, fill: CornerFill): Seq[Node] =
    Seq(
      div(cls := "card-title", title),
      div(cls := "card-desc", description),
    ) ++ cornerNodes(corners, fill)

  private def cornerNodes(c: CardCorners, fill: CornerFill): Seq[Node] =
    val mode = fillClass(fill)
    Seq(
      "card-corner-tl" -> c.topLeft,
      "card-corner-tr" -> c.topRight,
      "card-corner-bl" -> c.bottomLeft,
      "card-corner-br" -> c.bottomRight,
    ).collect { case (slot, text) if text.nonEmpty => div(cls := Seq("card-corner", slot, mode), text) }

  private def cornerRadius(shape: CornerShape): String = shape match
    case CornerShape.Circle  => "50%"
    case CornerShape.Rounded => "5px"
    case CornerShape.Square  => "0"

  private def fillClass(fill: CornerFill): String = fill match
    case CornerFill.Fill    => "card-corner-fill"
    case CornerFill.Outline => "card-corner-outline"
    case CornerFill.None    => "card-corner-none"
