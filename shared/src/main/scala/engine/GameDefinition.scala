package engine

import upickle.default.ReadWriter

/** A complete, self-contained game: the three authored artifacts — catalog,
  * rulebook, setup — bundled with the identity that lets it live in a library.
  * `id` is the stable storage key; `name` is the human label. This is the unit
  * the client saves to browser storage, plays, edits, and exports/imports as a
  * single JSON file.
  */
case class GameDefinition(
  id: String,
  name: String,
  catalog: CardCatalog,
  rulebook: Rulebook,
  setup: GameSetup,
  layout: CardLayout = CardLayout.default, // the shared card template; defaulted so games authored before it load unchanged
) derives ReadWriter
