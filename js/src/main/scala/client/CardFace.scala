package client

import com.raquo.laminar.api.L.*
import engine.*

/** Renders a card's front from the shared [[CardLayout]], so the playtest board,
  * the inspect overlay, and the design-tab preview all draw cards the same way.
  *
  * A layout's dimensions and fill are global, so they ride on CSS custom
  * properties set once on a container ([[vars]]); the per-card accent and text
  * fill the positioned title/description boxes ([[boxes]]). Callers own the card
  * root element (its classes, drag handlers, data attributes) and just splice in
  * the style and children these return.
  */
object CardFace:

  /** CSS custom properties carrying a layout's base rectangle — size and
    * background — onto a container so every `.card` inside takes the authored
    * dimensions and fill. Set on the play screen and on the editor's preview.
    */
  def vars(layout: CardLayout): String =
    s"--card-w:${layout.width}px;--card-h:${layout.height}px;--card-bg:${layout.background}"

  /** The accent rule for a card front: a top border in the card's own colour,
    * laid over the layout's shared background. Applied to the card root.
    */
  def accent(color: String): String =
    s"border-top:4px solid $color"

  /** The positioned title and description boxes for a card front, placed per the
    * shared `layout`. Spliced into the card root alongside its drag handlers.
    */
  def boxes(layout: CardLayout, title: String, description: String): Seq[Node] =
    Seq(
      div(cls := "card-title", styleAttr := box(layout.title), title),
      div(cls := "card-desc", styleAttr := box(layout.description), description),
    )

  private def box(b: CardBox): String =
    s"left:${b.x}px;top:${b.y}px;width:${b.width}px;height:${b.height}px"
