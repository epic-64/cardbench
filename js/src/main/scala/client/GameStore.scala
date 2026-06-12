package client

import engine.*
import org.scalajs.dom
import upickle.default.{read, write}
import scala.util.Try

/** The library of game definitions, persisted in the browser's `localStorage`.
  *
  * The whole library lives under one key as a single JSON list, so a read or
  * write touches the entire set at once — fine for the handful of games a player
  * keeps, and free of the index-vs-entries desync a per-game scheme invites.
  *
  * On a browser that has never run the app the key is absent (distinct from a
  * user-emptied library, which is an empty list): `loadOrSeed` plants the sample
  * game in that one case so the library is never empty on first visit, yet a
  * deliberate "delete everything" stays deleted.
  */
object GameStore:
  private val key = "gigadev.library"

  /** Every stored game, in saved order; an unreadable or absent store reads as
    * empty rather than throwing.
    */
  def load(): List[GameDefinition] =
    Option(dom.window.localStorage.getItem(key))
      .flatMap(json => Try(read[List[GameDefinition]](json)).toOption)
      .getOrElse(Nil)

  /** The library as it stands, seeding the sample game only when the store has
    * never been written (a fresh browser), never when the user has emptied it.
    */
  def loadOrSeed(): List[GameDefinition] =
    Option(dom.window.localStorage.getItem(key)) match
      case None =>
        val seeded = List(SampleGame.definition)
        persist(seeded)
        seeded
      case Some(json) =>
        Try(read[List[GameDefinition]](json)).toOption.getOrElse(Nil)

  /** Insert or update a definition by id, returning the new library. An existing
    * id is replaced in place (keeping its position); a new id is appended.
    */
  def save(definition: GameDefinition): List[GameDefinition] =
    val existing = load()
    val updated =
      if existing.exists(_.id == definition.id)
      then existing.map(d => if d.id == definition.id then definition else d)
      else existing :+ definition
    persist(updated)
    updated

  /** Drop a definition by id, returning the new library. */
  def delete(id: String): List[GameDefinition] =
    val updated = load().filterNot(_.id == id)
    persist(updated)
    updated

  /** A fresh, collision-resistant id for a newly created game. */
  def freshId(): String = s"game-${System.currentTimeMillis()}"

  private def persist(defs: List[GameDefinition]): Unit =
    dom.window.localStorage.setItem(key, write(defs))

  // ── ongoing games ──────────────────────────────────────────────────────────
  // A game's *definition* (catalog, rules, setup) lives in the library above; its
  // live table — where the cards have ended up mid-play — is saved apart, one key
  // per game id. Only the mutable part (the stacks) is stored: the catalog and
  // rules are rebuilt from the definition on load, so editing a game never leaves
  // a saved table referring to a stale rulebook.

  private def gameKey(id: String): String = s"gigadev.game.$id"

  /** The saved live table for a game, if one is in progress; `None` when the game
    * has never been played (or was restarted) and an unreadable save reads as none.
    */
  def loadGame(id: String): Option[List[Stack]] =
    Option(dom.window.localStorage.getItem(gameKey(id)))
      .flatMap(json => Try(read[List[Stack]](json)).toOption)

  /** Record the current live table for a game, overwriting any earlier save. */
  def saveGame(id: String, stacks: List[Stack]): Unit =
    dom.window.localStorage.setItem(gameKey(id), write(stacks))

  /** Forget a game's saved table, so the next play starts from a fresh setup. */
  def clearGame(id: String): Unit =
    dom.window.localStorage.removeItem(gameKey(id))
