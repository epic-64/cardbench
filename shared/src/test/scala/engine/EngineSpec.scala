package engine

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import upickle.default.{read, write}

class EngineSpec extends AnyWordSpec with Matchers:

  // A two-definition catalog and a table: a face-down deck of 12 and an empty,
  // face-up discard. Shared, immutable fixtures — no per-test state.
  private val catalog = CardCatalog(
    List(
      CardDef(CardDefId("build"), "#3b82f6", "Build", "Ship a feature."),
      CardDef(CardDefId("refactor"), "#22c55e", "Refactor", "Trash 1 Debt."),
    ),
  )
  private val setup = GameSetup(
    List(
      StackSpec(
        StackId("deck"),
        "Dev Deck",
        Position(1, 1),
        Facing.Down,
        List(SpawnSpec(CardDefId("build"), 8), SpawnSpec(CardDefId("refactor"), 4)),
      ),
      StackSpec(StackId("discard"), "Discard", Position(3, 1), Facing.Up, Nil),
    ),
  )

  private val deck    = StackId("deck")
  private val discard = StackId("discard")

  private def stackOf(state: GameState, id: StackId): Stack =
    state.stacks.find(_.id == id).get

  "Engine.setup" should:

    "spawn each definition's instances into its stack" in:
      val state = Engine.setup(catalog, setup)
      stackOf(state, deck).cards.size shouldBe 12
      stackOf(state, discard).cards shouldBe empty

    "assign deterministic ids and the stack's facing" in:
      val state = Engine.setup(catalog, setup)
      stackOf(state, deck).cards.head shouldBe CardInstance(CardId("deck#0"), CardDefId("build"), Facing.Down)
      stackOf(state, deck).cards.last shouldBe CardInstance(CardId("deck#11"), CardDefId("refactor"), Facing.Down)

  "Engine.shuffle" should:

    "reproduce the same order for the same seed" in:
      val state = Engine.setup(catalog, setup)
      Engine.shuffle(state, deck, 42L) shouldBe Engine.shuffle(state, deck, 42L)

    "keep the stack's multiset of cards unchanged" in:
      val state    = Engine.setup(catalog, setup)
      val shuffled = Engine.shuffle(state, deck, 7L).toOption.get
      stackOf(shuffled, deck).cards.map(_.id.value).sorted shouldBe stackOf(state, deck).cards.map(_.id.value).sorted

    "reject an unknown stack" in:
      Engine.shuffle(Engine.setup(catalog, setup), StackId("nope"), 1L) shouldBe Left(
        EngineError.UnknownStack(StackId("nope")),
      )

  "Engine.deal" should:

    "move the top card of the source onto the target" in:
      val state = Engine.setup(catalog, setup)
      val dealt = Engine.deal(state, deck, discard).toOption.get
      stackOf(dealt, deck).cards.size shouldBe 11
      stackOf(dealt, discard).cards.head shouldBe CardInstance(CardId("deck#0"), CardDefId("build"), Facing.Down)

    "reject dealing from an empty stack" in:
      Engine.deal(Engine.setup(catalog, setup), discard, deck) shouldBe Left(EngineError.EmptyStack(discard))

  "Engine.move" should:

    "land the named instance on top of the target" in:
      val state = Engine.setup(catalog, setup)
      val moved = Engine.move(state, CardId("deck#5"), discard).toOption.get
      stackOf(moved, discard).cards.head.id shouldBe CardId("deck#5")
      stackOf(moved, deck).cards.exists(_.id == CardId("deck#5")) shouldBe false

    "reject an unknown card" in:
      Engine.move(Engine.setup(catalog, setup), CardId("ghost"), discard) shouldBe Left(
        EngineError.UnknownCard(CardId("ghost")),
      )

  "Engine.flip" should:

    "turn a face-down card face-up and leave the rest alone" in:
      val state   = Engine.setup(catalog, setup)
      val flipped = Engine.flip(state, CardId("deck#0")).toOption.get
      stackOf(flipped, deck).cards.head.facing shouldBe Facing.Up
      stackOf(flipped, deck).cards.tail.map(_.facing).distinct shouldBe List(Facing.Down)

  "JSON codecs" should:

    "round-trip a catalog unchanged" in:
      read[CardCatalog](write(catalog)) shouldBe catalog

    "round-trip a setup unchanged" in:
      read[GameSetup](write(setup)) shouldBe setup
