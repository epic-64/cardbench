package client

import com.raquo.laminar.api.L.*
import engine.*

/** Throwaway playtest shell: one `Var[GameState]`, every verb an `update`. Just
  * enough to see and poke the engine in the browser.
  */
object GameView:

  def view(): Element =
    val initial = Engine.setup(SampleGame.catalog, SampleGame.setup)
    val state   = Var(initial)
    val catalog = initial.catalog

    div(
      cls := "game",
      h1("Card Engine — Playtest Table"),
      p(cls := "game-hint", "Click a card to flip it. Shuffle a deck, then deal cards onto another stack."),
      div(
        cls := "table",
        children <-- state.signal.map(_.stacks).distinct.map(_.map(stackView(state, catalog, _))),
      ),
    )

  private def stackView(state: Var[GameState], catalog: Map[CardDefId, CardDef], stack: Stack): Element =
    val targets = state.now().stacks.filterNot(_.id == stack.id)
    div(
      cls       := "stack",
      styleAttr := s"grid-column:${stack.position.col};grid-row:${stack.position.row}",
      div(cls := "stack-label", s"${stack.label} (${stack.cards.size})"),
      topView(state, catalog, stack),
      div(
        cls := "stack-controls",
        button(
          "Shuffle",
          onClick --> (_ => state.update(s => Engine.shuffle(s, stack.id, System.currentTimeMillis()).getOrElse(s))),
        ),
        targets.map: t =>
          button(s"→ ${t.label}", onClick --> (_ => state.update(s => Engine.deal(s, stack.id, t.id).getOrElse(s)))),
      ),
    )

  private def topView(state: Var[GameState], catalog: Map[CardDefId, CardDef], stack: Stack): Element =
    stack.cards.headOption match
      case None => div(cls := "card card-empty", "—")
      case Some(card) =>
        val flip = onClick --> (_ => state.update(s => Engine.flip(s, card.id).getOrElse(s)))
        card.facing match
          case Facing.Down => div(cls := "card card-back", flip)
          case Facing.Up =>
            val d = catalog.get(card.defId)
            div(
              cls       := "card card-front",
              styleAttr := d.map(c => s"border-top:4px solid ${c.color}").getOrElse(""),
              div(cls := "card-title", d.map(_.title).getOrElse(card.defId.value)),
              div(cls := "card-desc", d.map(_.description).getOrElse("")),
              flip,
            )
