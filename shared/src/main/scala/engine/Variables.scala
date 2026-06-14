package engine

import scala.util.matching.Regex

/** Variables that card text can interpolate. A card's title, description, or corner
  * slots may contain `%name%` tokens; at render time each token is replaced with the
  * variable's current value, read from the live table (see `Engine`). Only built-in
  * variables exist for now — `current_player_index` tracks whose turn it is — so a
  * card can read `Player %current_player_index%` and follow the turn as it passes.
  */
object Variables:

  /** A variable the author can reference: its `name` (the `%name%` token), a human
    * `description` for the editor, and how its `value` is read off the table.
    */
  case class Variable(name: String, description: String, value: GameState => String)

  /** Every always-present variable. Listed in the editor so an author knows which
    * tokens exist; `value` resolves each against a concrete table.
    */
  val builtins: List[Variable] = List(
    Variable("current_player_index", "The 0-based index of the player whose turn it is.", _.currentPlayer.toString),
  )

  /** The current value of every variable on this table, keyed by name. */
  def values(state: GameState): Map[String, String] =
    builtins.iterator.map(v => v.name -> v.value(state)).toMap

  private val token: Regex = "%([A-Za-z0-9_]+)%".r

  /** Replace every `%name%` token in `text` with its value on this table. An unknown
    * token is left exactly as written, so a typo stays visible rather than vanishing.
    * Text with no `%` short-circuits, so the common case pays nothing.
    */
  def interpolate(text: String, state: GameState): String =
    if !text.contains('%') then text
    else
      val vals = values(state)
      token.replaceAllIn(text, m => Regex.quoteReplacement(vals.getOrElse(m.group(1), m.matched)))

  /** A copy of `card` with every interpolatable text field — title, description, and
    * the three corner slots — resolved against the table, ready to render.
    */
  def resolve(card: CardDef, state: GameState): CardDef =
    card.copy(
      title = interpolate(card.title, state),
      description = interpolate(card.description, state),
      corners = CardCorners(
        topRight = interpolate(card.corners.topRight, state),
        bottomLeft = interpolate(card.corners.bottomLeft, state),
        bottomRight = interpolate(card.corners.bottomRight, state),
      ),
    )
