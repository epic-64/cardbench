package client

import com.raquo.laminar.api.L.*
import engine.*

/** The whole client as three screens over the game library: browse it, play one
  * game, or edit one. A single `Var[Screen]` is the router; the library lives in
  * its own `Var` so a save or delete on any screen reflects straight back into
  * the list. The screens themselves stay decoupled, navigating only through the
  * callbacks handed to them here.
  */
object AppView:
  enum Screen:
    case Library
    case Play(definition: GameDefinition)
    case Edit(definition: GameDefinition)
    case FineTune(definition: GameDefinition)

  def view(): Element =
    val library = Var(GameStore.loadOrSeed())
    val screen  = Var[Screen](Screen.Library)

    div(
      cls := "app-shell",
      child <-- screen.signal.map {
        case Screen.Library =>
          LibraryView.view(
            library,
            onPlay = d => screen.set(Screen.Play(d)),
            onEdit = d => screen.set(Screen.Edit(d)),
            onFineTune = d => screen.set(Screen.FineTune(d)),
          )
        case Screen.Play(d) =>
          GameView.view(d, () => screen.set(Screen.Library))
        case Screen.Edit(d) =>
          EditorView.view(
            d,
            onSave = saved =>
              library.set(GameStore.save(saved))
              screen.set(Screen.Library),
            onCancel = () => screen.set(Screen.Library),
          )
        case Screen.FineTune(d) =>
          FineTuneView.view(
            d,
            onSave = saved =>
              library.set(GameStore.save(saved))
              screen.set(Screen.Library),
            onCancel = () => screen.set(Screen.Library),
          )
      },
    )
