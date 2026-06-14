package engine

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class VariablesSpec extends AnyWordSpec with Matchers:

  // A bare table is enough — only `currentPlayer` is read; the catalog/rules/stacks
  // are irrelevant to variable interpolation.
  private def tableOn(player: Int): GameState =
    GameState(Map.empty, Nil, Nil, currentPlayer = player, players = 4)

  "Variables.interpolate" should:

    "replace the current_player_index token with the live value" in:
      Variables.interpolate("Player %current_player_index%", tableOn(2)) shouldBe "Player 2"

    "track the value as the turn passes" in:
      Variables.interpolate("%current_player_index%", tableOn(0)) shouldBe "0"
      Variables.interpolate("%current_player_index%", tableOn(3)) shouldBe "3"

    "replace every occurrence of a token" in:
      Variables.interpolate("%current_player_index%-%current_player_index%", tableOn(1)) shouldBe "1-1"

    "leave an unknown token untouched" in:
      Variables.interpolate("score: %gold%", tableOn(0)) shouldBe "score: %gold%"

    "return text with no tokens unchanged" in:
      Variables.interpolate("plain text", tableOn(0)) shouldBe "plain text"

  "Variables.builtins" should:

    "always offer current_player_index" in:
      Variables.builtins.map(_.name) should contain("current_player_index")

  "Variables.resolve" should:

    "interpolate a card's title, description, and corners" in:
      val card = CardDef(
        CardDefId("turn"),
        "#888888",
        "Turn of P%current_player_index%",
        "It is player %current_player_index%'s turn.",
        CardCorners(topRight = "%current_player_index%"),
      )
      val resolved = Variables.resolve(card, tableOn(2))
      resolved.title shouldBe "Turn of P2"
      resolved.description shouldBe "It is player 2's turn."
      resolved.corners.topRight shouldBe "2"
