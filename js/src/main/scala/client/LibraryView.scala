package client

import com.raquo.laminar.api.L.*
import engine.*
import org.scalajs.dom
import upickle.default.{read, write}
import scala.util.Try

/** The home screen: every saved game as a card with its actions — play, edit,
  * export to a file, delete — plus a header to start a new game or import one
  * from a file. Mutating actions write through `GameStore` and push the result
  * back into the shared `library` Var; navigating ones call up to `AppView`.
  */
object LibraryView:

  def view(
    library: Var[List[GameDefinition]],
    onPlay: GameDefinition => Unit,
    onEdit: GameDefinition => Unit,
  ): Element =

    // Import as a brand-new entry (fresh id) so bringing a file in never silently
    // overwrites an existing game; a malformed file is reported, not swallowed.
    def importText(text: String): Unit =
      Try(read[GameDefinition](text)).toOption match
        case Some(d) => library.set(GameStore.save(d.copy(id = GameStore.freshId())))
        case None    => dom.window.alert("Could not import: that file is not a valid game definition.")

    def exportGame(d: GameDefinition): Unit =
      FileTransfer.download(s"${slug(d.name)}.json", write(d, indent = 2))

    def deleteGame(d: GameDefinition): Unit =
      if dom.window.confirm(s"""Delete "${d.name}"? This cannot be undone.""") then
        library.set(GameStore.delete(d.id))

    def gameRow(d: GameDefinition): Element =
      div(
        cls := "game-card",
        div(
          cls := "game-card-info",
          div(cls := "game-card-name", d.name),
          div(
            cls := "game-card-meta",
            s"${d.catalog.cards.size} cards · ${d.rulebook.rules.size} rules · ${d.setup.stacks.size} stacks",
          ),
        ),
        div(
          cls := "game-card-actions",
          button(cls := "btn btn-primary", "Play", onClick --> (_ => onPlay(d))),
          button(cls := "btn", "Edit", onClick --> (_ => onEdit(d))),
          button(cls := "btn", "Export", onClick --> (_ => exportGame(d))),
          button(cls := "btn btn-danger", "Delete", onClick --> (_ => deleteGame(d))),
        ),
      )

    div(
      cls := "library",
      div(
        cls := "library-header",
        h1("Games"),
        div(
          cls := "library-header-actions",
          button(
            cls := "btn btn-primary",
            "+ New game",
            onClick --> (_ => onEdit(blankGame())),
          ),
          importButton(importText),
        ),
      ),
      child <-- library.signal.map {
        case Nil   => div(cls := "library-empty", "No games yet. Create one or import a file.")
        case games => div(cls := "library-list", games.map(gameRow))
      },
    )

  // A styled file picker: the hidden input does the work, the label is the button.
  // Resetting the input's value after each pick lets the same file import twice.
  private def importButton(onText: String => Unit): Element =
    label(
      cls := "btn",
      "Import",
      input(
        typ          := "file",
        cls          := "visually-hidden",
        onChange --> { e =>
          val el = e.target.asInstanceOf[dom.html.Input]
          Option(el.files).filter(_.length > 0).map(_(0)).foreach(f => FileTransfer.readText(f, onText))
          el.value = ""
        },
      ),
    )

  // A blank starting point for "New game": an empty catalog, rulebook, and table
  // for the editor to fill in.
  private def blankGame(): GameDefinition =
    GameDefinition(GameStore.freshId(), "New Game", CardCatalog(Nil), Rulebook(Nil), GameSetup(Nil))

  // A filesystem-friendly stem derived from the game's name, never empty.
  private def slug(name: String): String =
    val cleaned = name.toLowerCase.replaceAll("[^a-z0-9]+", "-").replaceAll("(^-+)|(-+$)", "")
    if cleaned.isEmpty then "game" else cleaned
