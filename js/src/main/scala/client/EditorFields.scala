package client

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** The small vocabulary of labelled form controls the editor is built from.
  *
  * Every field is *uncontrolled*: it takes its starting value once (so editing
  * it never fights a re-render and never loses the caret) and reports each change
  * through a callback. The editor re-creates a field's row only when items are
  * added or removed, at which point the fresh field picks up the current value —
  * so the displayed value and the model stay in step without binding `value` to a
  * signal on every keystroke.
  */
object EditorFields:

  def textField(name: String, initial: String, onChanged: String => Unit): Element =
    field(
      name,
      input(
        cls := "field-input",
        typ := "text",
        defaultValue := initial,
        onInput.mapToValue --> (v => onChanged(v)),
      ),
    )

  def textAreaField(name: String, initial: String, onChanged: String => Unit): Element =
    field(
      name,
      textArea(
        cls := "field-input field-textarea",
        rows := 4,
        defaultValue := initial,
        onInput.mapToValue --> (v => onChanged(v)),
      ),
    )

  def numberField(name: String, initial: Int, onChanged: Int => Unit): Element =
    field(
      name,
      input(
        cls := "field-input field-number",
        typ := "number",
        defaultValue := initial.toString,
        onInput.mapToValue --> (v => v.toIntOption.foreach(onChanged)),
      ),
    )

  def colorField(name: String, initial: String, onChanged: String => Unit): Element =
    field(
      name,
      input(
        cls := "field-input field-color",
        typ := "color",
        defaultValue := initial,
        onInput.mapToValue --> (v => onChanged(v)),
      ),
    )

  def checkboxField(name: String, initial: Boolean, onChanged: Boolean => Unit): Element =
    label(
      cls := "field field-check",
      input(
        typ := "checkbox",
        defaultChecked := initial,
        onInput --> (e => onChanged(e.target.asInstanceOf[dom.html.Input].checked)),
      ),
      span(cls := "field-label", name),
    )

  /** A drop-down over a fixed set of `(label, value)` choices. The current value's
    * option is pre-selected; picking another reports its value.
    */
  def selectField[A](name: String, options: List[(String, A)], current: A, onChanged: A => Unit): Element =
    field(
      name,
      select(
        cls := "field-input",
        onChange.mapToValue --> (picked => options.find(_._1 == picked).foreach((_, a) => onChanged(a))),
        options.map((labelText, a) => option(value := labelText, selected := (a == current), labelText)),
      ),
    )

  /** A drop-down over a *live* set of ids — the catalog's card ids or the table's
    * stack ids — so picking a reference can't typo a name that doesn't exist. Both
    * the choices and the selected value are signals: the list reshapes as ids are
    * added, removed, or renamed, and the box always reflects the stored value. A
    * stale value (e.g. a reference to a since-deleted id) is kept as its own choice
    * so it stays visible rather than silently snapping to another id.
    */
  def idSelectField(
    name: String,
    options: Signal[List[String]],
    current: Signal[String],
    onChanged: String => Unit,
  ): Element =
    field(
      name,
      select(
        cls := "field-input",
        onChange.mapToValue --> (v => onChanged(v)),
        children <-- options.combineWith(current).map: (opts, cur) =>
          val all = if opts.contains(cur) then opts else cur :: opts
          all.map(o => option(value := o, selected := (o == cur), if o.isEmpty then "(none)" else o)),
      ),
    )

  def addButton(text: String, onAdd: () => Unit): Element =
    button(cls := "btn btn-add", text, onClick --> (_ => onAdd()))

  def removeButton(onRemove: () => Unit): Element =
    button(cls := "btn btn-danger btn-icon", title := "Remove", "✕", onClick --> (_ => onRemove()))

  private def field(name: String, control: Element): Element =
    label(cls := "field", span(cls := "field-label", name), control)
