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

  // The effect system, authored apart from the catalog. Build deals 2 debt into
  // the discard and draws 1 feature into the building zone; with no move out of
  // the play zone it simply stays where it was played. Every other kind is inert.
  // Shared across the setups below — plays only ever fire in the dedicated play
  // fixtures, so it is harmless elsewhere.
  private val rulebook = Rulebook(
    List(
      CardRule(
        CardDefId("build"),
        effects = List(
          Effect.Deal(StackId("features"), StackId("build-zone"), 1, reveal = true),
          Effect.Deal(StackId("debt"), StackId("discard"), 2, reveal = true),
        ),
      ),
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

  // The same deck, but flagged to shuffle at setup.
  private val shuffledSetup = GameSetup(
    List(
      StackSpec(
        StackId("deck"),
        "Dev Deck",
        Position(1, 1),
        Facing.Down,
        List(SpawnSpec(CardDefId("build"), 8), SpawnSpec(CardDefId("refactor"), 4)),
        arrangement = Arrangement.Shuffled,
      ),
    ),
  )

  private val deck    = StackId("deck")
  private val discard = StackId("discard")

  // Two single-card stacks, for exercising "a stack ceases to exist at 0 cards".
  private val solo = GameSetup(
    List(
      StackSpec(StackId("a"), "A", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("build"), 1))),
      StackSpec(StackId("b"), "B", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("build"), 1))),
    ),
  )

  // A "build" card that, when played, deals 2 debt into the discard pile and
  // draws 1 feature into the building zone — mirroring the engine's headline
  // example — plus a table with those source and (persistent, empty) target piles.
  private val playCatalog = CardCatalog(
    List(
      CardDef(CardDefId("build"), "#3b82f6", "Build", "Ship a feature."),
      CardDef(CardDefId("debt"), "#6b7280", "Tech Debt", "Dead weight."),
      CardDef(CardDefId("feature"), "#16a34a", "Feature", "A shipped feature."),
    ),
  )
  private val playSetup = GameSetup(
    List(
      StackSpec(StackId("hand"), "Hand", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("build"), 1))),
      StackSpec(StackId("debt"), "Debt", Position(0, 0), Facing.Down, List(SpawnSpec(CardDefId("debt"), 5))),
      StackSpec(StackId("features"), "Features", Position(0, 0), Facing.Down, List(SpawnSpec(CardDefId("feature"), 3))),
      StackSpec(StackId("discard"), "Discard", Position(0, 0), Facing.Up, Nil, persistent = true),
      StackSpec(StackId("build-zone"), "Building", Position(0, 0), Facing.Up, Nil, persistent = true, layout = Layout.Row),
      StackSpec(StackId("in-play"), "In Play", Position(0, 0), Facing.Up, Nil, persistent = true),
    ),
  )

  // The same table, but the debt pile holds fewer cards than Build tries to deal,
  // so resolving its effects must fail partway and roll back.
  private val drained = GameSetup(
    List(
      StackSpec(StackId("hand"), "Hand", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("build"), 1))),
      StackSpec(StackId("debt"), "Debt", Position(0, 0), Facing.Down, List(SpawnSpec(CardDefId("debt"), 1)), persistent = true),
      StackSpec(StackId("features"), "Features", Position(0, 0), Facing.Down, List(SpawnSpec(CardDefId("feature"), 3))),
      StackSpec(StackId("discard"), "Discard", Position(0, 0), Facing.Up, Nil, persistent = true),
      StackSpec(StackId("build-zone"), "Building", Position(0, 0), Facing.Up, Nil, persistent = true),
      StackSpec(StackId("in-play"), "In Play", Position(0, 0), Facing.Up, Nil, persistent = true),
    ),
  )

  // One persistent pile and one ordinary pile, each holding a single card, to
  // show only the ordinary one disappears once emptied.
  private val essential = GameSetup(
    List(
      StackSpec(StackId("a"), "A", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("build"), 1)), persistent = true),
      StackSpec(StackId("b"), "B", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("build"), 1))),
    ),
  )

  private def stackOf(state: GameState, id: StackId): Stack =
    state.stacks.find(_.id == id).get

  "Engine.setup" should:

    "spawn each definition's instances into its stack" in:
      val state = Engine.setup(catalog, rulebook, setup)
      stackOf(state, deck).cards.size shouldBe 12
      stackOf(state, discard).cards shouldBe empty

    "assign deterministic ids and the stack's facing" in:
      val state = Engine.setup(catalog, rulebook, setup)
      stackOf(state, deck).cards.head shouldBe CardInstance(CardId("deck#0"), CardDefId("build"), Facing.Down)
      stackOf(state, deck).cards.last shouldBe CardInstance(CardId("deck#11"), CardDefId("refactor"), Facing.Down)

    "leave an unshuffled stack in spawn order regardless of seed" in:
      Engine.setup(catalog, rulebook, setup, 99L) shouldBe Engine.setup(catalog, rulebook, setup, 1L)

    "order a shuffled stack deterministically for a given seed" in:
      Engine.setup(catalog, rulebook, shuffledSetup, 42L) shouldBe Engine.setup(catalog, rulebook, shuffledSetup, 42L)

    "keep a shuffled stack's cards unchanged, only reordering them" in:
      val shuffled   = stackOf(Engine.setup(catalog, rulebook, shuffledSetup, 7L), deck)
      val unshuffled = stackOf(Engine.setup(catalog, rulebook, setup, 7L), deck)
      shuffled.cards.toSet shouldBe unshuffled.cards.toSet

    "flag a shuffled stack and leave an ordered one unflagged" in:
      stackOf(Engine.setup(catalog, rulebook, shuffledSetup, 1L), deck).shuffled shouldBe true
      stackOf(Engine.setup(catalog, rulebook, setup), deck).shuffled shouldBe false

    "carry a stack's layout onto the table" in:
      stackOf(Engine.setup(playCatalog, rulebook, playSetup), StackId("build-zone")).layout shouldBe Layout.Row

  "Engine.shuffle" should:

    "reproduce the same order for the same seed" in:
      val state = Engine.setup(catalog, rulebook, setup)
      Engine.shuffle(state, deck, 42L) shouldBe Engine.shuffle(state, deck, 42L)

    "keep the stack's multiset of cards unchanged" in:
      val state    = Engine.setup(catalog, rulebook, setup)
      val shuffled = Engine.shuffle(state, deck, 7L).toOption.get
      stackOf(shuffled, deck).cards.map(_.id.value).sorted shouldBe stackOf(state, deck).cards.map(_.id.value).sorted

    "flag the reordered stack as shuffled" in:
      val shuffled = Engine.shuffle(Engine.setup(catalog, rulebook, setup), deck, 7L).toOption.get
      stackOf(shuffled, deck).shuffled shouldBe true

    "reject an unknown stack" in:
      Engine.shuffle(Engine.setup(catalog, rulebook, setup), StackId("nope"), 1L) shouldBe Left(
        EngineError.UnknownStack(StackId("nope")),
      )

  "Engine.deal" should:

    "move the top card of the source onto the target" in:
      val state = Engine.setup(catalog, rulebook, setup)
      val dealt = Engine.deal(state, deck, discard).toOption.get
      stackOf(dealt, deck).cards.size shouldBe 11
      stackOf(dealt, discard).cards.head shouldBe CardInstance(CardId("deck#0"), CardDefId("build"), Facing.Down)

    "reject dealing from an empty stack" in:
      Engine.deal(Engine.setup(catalog, rulebook, setup), discard, deck) shouldBe Left(EngineError.EmptyStack(discard))

    "clear the shuffled flag on a stack it draws from" in:
      val shuffled = Engine.shuffle(Engine.setup(catalog, rulebook, setup), deck, 7L).toOption.get
      stackOf(Engine.deal(shuffled, deck, discard).toOption.get, deck).shuffled shouldBe false

  "Engine.move" should:

    "land the named instance on top of the target" in:
      val state = Engine.setup(catalog, rulebook, setup)
      val moved = Engine.move(state, CardId("deck#5"), discard).toOption.get
      stackOf(moved, discard).cards.head.id shouldBe CardId("deck#5")
      stackOf(moved, deck).cards.exists(_.id == CardId("deck#5")) shouldBe false

    "reject an unknown card" in:
      Engine.move(Engine.setup(catalog, rulebook, setup), CardId("ghost"), discard) shouldBe Left(
        EngineError.UnknownCard(CardId("ghost")),
      )

    "remove the source stack once its last card leaves" in:
      val moved = Engine.move(Engine.setup(catalog, rulebook, solo), CardId("a#0"), StackId("b")).toOption.get
      moved.stacks.exists(_.id == StackId("a")) shouldBe false
      stackOf(moved, StackId("b")).cards.size shouldBe 2

  "Engine.flip" should:

    "turn a face-down card face-up and leave the rest alone" in:
      val state   = Engine.setup(catalog, rulebook, setup)
      val flipped = Engine.flip(state, CardId("deck#0")).toOption.get
      stackOf(flipped, deck).cards.head.facing shouldBe Facing.Up
      stackOf(flipped, deck).cards.tail.map(_.facing).distinct shouldBe List(Facing.Down)

  "Engine.moveStack" should:

    "reposition the stack while keeping its cards" in:
      val state = Engine.setup(catalog, rulebook, setup)
      val moved = Engine.moveStack(state, deck, Position(200, 80)).toOption.get
      stackOf(moved, deck).position shouldBe Position(200, 80)
      stackOf(moved, deck).cards.size shouldBe 12

    "keep the shuffled flag when only repositioning" in:
      val shuffled = Engine.shuffle(Engine.setup(catalog, rulebook, setup), deck, 7L).toOption.get
      stackOf(Engine.moveStack(shuffled, deck, Position(9, 9)).toOption.get, deck).shuffled shouldBe true

    "reject an unknown stack" in:
      Engine.moveStack(Engine.setup(catalog, rulebook, setup), StackId("nope"), Position(0, 0)) shouldBe Left(
        EngineError.UnknownStack(StackId("nope")),
      )

  "Engine.extractCard" should:

    "pull the card into a new single-card stack at the given position" in:
      val state = Engine.setup(catalog, rulebook, setup)
      val out   = Engine.extractCard(state, CardId("deck#3"), StackId("loose"), Position(50, 90)).toOption.get
      stackOf(out, StackId("loose")).cards.map(_.id) shouldBe List(CardId("deck#3"))
      stackOf(out, StackId("loose")).position shouldBe Position(50, 90)
      stackOf(out, deck).cards.exists(_.id == CardId("deck#3")) shouldBe false

    "reject an unknown card" in:
      Engine.extractCard(Engine.setup(catalog, rulebook, setup), CardId("ghost"), StackId("loose"), Position(0, 0)) shouldBe Left(
        EngineError.UnknownCard(CardId("ghost")),
      )

    "remove the source stack when extracting its only card" in:
      val out = Engine.extractCard(Engine.setup(catalog, rulebook, solo), CardId("a#0"), StackId("loose"), Position(5, 5)).toOption.get
      out.stacks.exists(_.id == StackId("a")) shouldBe false
      stackOf(out, StackId("loose")).cards.map(_.id) shouldBe List(CardId("a#0"))

  "A persistent stack" should:

    "stay on the table, empty, once its last card leaves" in:
      val moved = Engine.move(Engine.setup(catalog, rulebook, essential), CardId("a#0"), StackId("b")).toOption.get
      moved.stacks.exists(_.id == StackId("a")) shouldBe true
      stackOf(moved, StackId("a")).cards shouldBe empty

    "still let an ordinary stack vanish when emptied" in:
      val moved = Engine.move(Engine.setup(catalog, rulebook, essential), CardId("b#0"), StackId("a")).toOption.get
      moved.stacks.exists(_.id == StackId("b")) shouldBe false

  "Engine.play" should:

    "move the played card into the play zone and resolve its effects" in:
      val state  = Engine.setup(playCatalog, rulebook, playSetup)
      val played = Engine.play(state, CardId("hand#0"), StackId("in-play")).toOption.get
      stackOf(played, StackId("discard")).cards.size shouldBe 2
      stackOf(played, StackId("build-zone")).cards.size shouldBe 1
      stackOf(played, StackId("debt")).cards.size shouldBe 3
      stackOf(played, StackId("features")).cards.size shouldBe 2
      stackOf(played, StackId("in-play")).cards.map(_.id) shouldBe List(CardId("hand#0"))

    "reject an unknown card" in:
      Engine.play(Engine.setup(playCatalog, rulebook, playSetup), CardId("ghost"), StackId("in-play")) shouldBe Left(
        EngineError.UnknownCard(CardId("ghost")),
      )

    "leave the table untouched when an effect over-draws its source" in:
      val state = Engine.setup(playCatalog, rulebook, drained)
      Engine.play(state, CardId("hand#0"), StackId("in-play")) shouldBe Left(EngineError.EmptyStack(StackId("debt")))

    "turn the revealed cards face-up where they land" in:
      val played = Engine.play(Engine.setup(playCatalog, rulebook, playSetup), CardId("hand#0"), StackId("in-play")).toOption.get
      stackOf(played, StackId("build-zone")).cards.head.facing shouldBe Facing.Up
      stackOf(played, StackId("discard")).cards.map(_.facing).distinct shouldBe List(Facing.Up)

  "Engine.playSteps" should:

    "script the play move first, then each effect move and reveal in order" in:
      Engine.playSteps(Engine.setup(playCatalog, rulebook, playSetup), CardId("hand#0"), StackId("in-play")) shouldBe Right(
        List(
          Step.Move(CardId("hand#0"), StackId("in-play")),
          Step.Move(CardId("features#0"), StackId("build-zone")),
          Step.Flip(CardId("features#0")),
          Step.Move(CardId("debt#0"), StackId("discard")),
          Step.Flip(CardId("debt#0")),
          Step.Move(CardId("debt#1"), StackId("discard")),
          Step.Flip(CardId("debt#1")),
        ),
      )

  "JSON codecs" should:

    "round-trip a catalog unchanged" in:
      read[CardCatalog](write(catalog)) shouldBe catalog

    "round-trip a setup unchanged" in:
      read[GameSetup](write(setup)) shouldBe setup

    "round-trip a rulebook of card effects unchanged" in:
      read[Rulebook](write(rulebook)) shouldBe rulebook

    "round-trip a setup with persistent stacks unchanged" in:
      read[GameSetup](write(playSetup)) shouldBe playSetup
