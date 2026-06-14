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

  // The effect system, authored apart from the catalog. When a Build card comes
  // to rest on the "in-play" stack it draws 1 feature into the building zone and
  // deals 2 debt into the discard; with no move out of in-play it then stays put.
  // Every other event is inert. Shared across the setups below — it only fires via
  // `drop` onto in-play, which only the dedicated play fixtures do, so it is
  // harmless elsewhere.
  private val rulebook = Rulebook(
    List(
      Rule(
        Trigger.Moved(CardDefId("build"), StackRef("in-play")),
        effects = List(
          Effect.Deal(StackRef("features"), StackRef("build-zone"), 1),
          Effect.Deal(StackRef("debt"), StackRef("discard"), 2),
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

  // A rulebook whose single reaction shuffles the debt pile when a Build lands on
  // in-play — exercising the Shuffle effect in isolation from any deal.
  private val shuffleRulebook = Rulebook(
    List(
      Rule(
        Trigger.Moved(CardDefId("build"), StackRef("in-play")),
        effects = List(Effect.Shuffle(StackRef("debt"))),
      ),
    ),
  )

  // A rulebook whose reaction to a Build landing on in-play pauses for the player:
  // a Manual step sits between two deals, so the second deal must run only once the
  // player has marked the manual step done — and against whatever table they leave.
  private val manualPrompt = "Resolve the battle by hand."
  private val manualRulebook = Rulebook(
    List(
      Rule(
        Trigger.Moved(CardDefId("build"), StackRef("in-play")),
        effects = List(
          Effect.Manual(manualPrompt),
          Effect.Deal(StackRef("features"), StackRef("build-zone"), 1),
        ),
      ),
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
      StackSpec(StackId("a"), "A", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("build"), 1)), persistent = false),
      StackSpec(StackId("b"), "B", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("build"), 1)), persistent = false),
    ),
  )

  // The catalog and table for exercising the effect system: a Build card (whose
  // rule, above, fires when it lands on in-play), the debt and feature source
  // piles its effects draw from, and the persistent, empty target piles they fill.
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
      StackSpec(StackId("b"), "B", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("build"), 1)), persistent = false),
    ),
  )

  // A two-player table for the player-relative effect system: a single shared Play
  // zone owned by nobody, a shared Debt source, and one deck per player — each
  // tagged with the shared role "deck". One rule (not one per player) sends 2 debt
  // to the *current* player's deck whenever a Build lands on the shared Play zone.
  private val ownerCatalog = CardCatalog(
    List(
      CardDef(CardDefId("build"), "#3b82f6", "Build", "Ship a feature."),
      CardDef(CardDefId("debt"), "#6b7280", "Tech Debt", "Dead weight."),
    ),
  )
  private val ownerRulebook = Rulebook(
    List(
      Rule(
        Trigger.Moved(CardDefId("build"), StackRef("play")),
        effects = List(Effect.Deal(StackRef("debt"), StackRef.Owned("deck"), 2)),
      ),
    ),
  )
  private val ownerSetup = GameSetup(
    List(
      StackSpec(StackId("hand"), "Hand", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("build"), 1))),
      StackSpec(StackId("play"), "Play", Position(0, 0), Facing.Up, Nil, persistent = true),
      StackSpec(StackId("debt"), "Debt", Position(0, 0), Facing.Down, List(SpawnSpec(CardDefId("debt"), 10))),
      StackSpec(StackId("deck1"), "Deck 1", Position(0, 0), Facing.Down, Nil, owner = Some(0), role = "deck"),
      StackSpec(StackId("deck2"), "Deck 2", Position(0, 0), Facing.Down, Nil, owner = Some(1), role = "deck"),
    ),
  )

  // A table for the trigger side of player-relative references: each player owns a
  // "gate" stack, and one rule fires only when a Build lands on the *current*
  // player's gate, dealing a token from a shared source into a shared sink so the
  // firing is observable.
  private val gateCatalog = CardCatalog(
    List(
      CardDef(CardDefId("build"), "#3b82f6", "Build", "x"),
      CardDef(CardDefId("token"), "#999999", "Token", "y"),
    ),
  )
  private val gateRulebook = Rulebook(
    List(
      Rule(
        Trigger.Moved(CardDefId("build"), StackRef.Owned("gate")),
        effects = List(Effect.Deal(StackRef("src"), StackRef("sink"), 1)),
      ),
    ),
  )
  private val gateSetup = GameSetup(
    List(
      StackSpec(StackId("hand"), "Hand", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("build"), 1))),
      StackSpec(StackId("src"), "Src", Position(0, 0), Facing.Up, List(SpawnSpec(CardDefId("token"), 5))),
      StackSpec(StackId("sink"), "Sink", Position(0, 0), Facing.Up, Nil, persistent = true),
      StackSpec(StackId("gate1"), "Gate 1", Position(0, 0), Facing.Up, Nil, persistent = true, owner = Some(0), role = "gate"),
      StackSpec(StackId("gate2"), "Gate 2", Position(0, 0), Facing.Up, Nil, persistent = true, owner = Some(1), role = "gate"),
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

    "start on the first player" in:
      Engine.setup(catalog, rulebook, setup).currentPlayer shouldBe 0

  "Engine.endTurn" should:

    "pass the turn to the next player" in:
      Engine.endTurn(Engine.setup(catalog, rulebook, setup, players = 3)).currentPlayer shouldBe 1

    "wrap back to the first player after the last" in:
      val lastPlayer = Engine.setup(catalog, rulebook, setup, players = 3).copy(currentPlayer = 2)
      Engine.endTurn(lastPlayer).currentPlayer shouldBe 0

    "leave a solitaire game on the first player" in:
      Engine.endTurn(Engine.setup(catalog, rulebook, setup, players = 1)).currentPlayer shouldBe 0

  "An EndTurn effect" should:

    // Drive a cascade to its settled table — EndTurn produces one bookkeeping step.
    def runAll(progress: Progress): GameState = progress match
      case Progress.Done(s)              => s
      case Progress.Ran(s, _, rest)      => runAll(Engine.step(s, rest, 0L).toOption.get)
      case Progress.Await(s, _, _, rest) => runAll(Engine.step(s, rest, 0L).toOption.get)

    "pass the turn when run from a button" in:
      val state = Engine.setup(catalog, rulebook, setup, players = 3)
      runAll(Engine.buttonCascade(state, List(Effect.EndTurn)).toOption.get).currentPlayer shouldBe 1

    "wrap to the first player after the last" in:
      val state = Engine.setup(catalog, rulebook, setup, players = 3).copy(currentPlayer = 2)
      runAll(Engine.buttonCascade(state, List(Effect.EndTurn)).toOption.get).currentPlayer shouldBe 0

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

  "Engine.flipStack" should:

    "turn every card in the stack to the opposite facing" in:
      val state   = Engine.setup(catalog, rulebook, setup)
      val flipped = Engine.flipStack(state, deck).toOption.get
      stackOf(flipped, deck).cards.map(_.facing).distinct shouldBe List(Facing.Up)

    "leave the pile order unchanged" in:
      val state   = Engine.setup(catalog, rulebook, setup)
      val flipped = Engine.flipStack(state, deck).toOption.get
      stackOf(flipped, deck).cards.map(_.id) shouldBe stackOf(state, deck).cards.map(_.id)

    "turn the stack's own facing over with its cards" in:
      val flipped = Engine.flipStack(Engine.setup(catalog, rulebook, setup), deck).toOption.get
      stackOf(flipped, deck).facing shouldBe Facing.Up

    "reject an unknown stack" in:
      Engine.flipStack(Engine.setup(catalog, rulebook, setup), StackId("nope")) shouldBe Left(
        EngineError.UnknownStack(StackId("nope")),
      )

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

  "Engine.drop" should:

    "relocate the card onto the stack and fire the rules its arrival triggers" in:
      val state  = Engine.setup(playCatalog, rulebook, playSetup)
      val played = Engine.drop(state, CardId("hand#0"), StackId("in-play")).toOption.get
      stackOf(played, StackId("discard")).cards.size shouldBe 2
      stackOf(played, StackId("build-zone")).cards.size shouldBe 1
      stackOf(played, StackId("debt")).cards.size shouldBe 3
      stackOf(played, StackId("features")).cards.size shouldBe 2
      stackOf(played, StackId("in-play")).cards.map(_.id) shouldBe List(CardId("hand#0"))

    "reject an unknown card" in:
      Engine.drop(Engine.setup(playCatalog, rulebook, playSetup), CardId("ghost"), StackId("in-play")) shouldBe Left(
        EngineError.UnknownCard(CardId("ghost")),
      )

    "leave the table untouched when an effect over-draws its source" in:
      val state = Engine.setup(playCatalog, rulebook, drained)
      Engine.drop(state, CardId("hand#0"), StackId("in-play")) shouldBe Left(EngineError.EmptyStack(StackId("debt")))

    "turn the revealed cards face-up where they land" in:
      val played = Engine.drop(Engine.setup(playCatalog, rulebook, playSetup), CardId("hand#0"), StackId("in-play")).toOption.get
      stackOf(played, StackId("build-zone")).cards.head.facing shouldBe Facing.Up
      stackOf(played, StackId("discard")).cards.map(_.facing).distinct shouldBe List(Facing.Up)

    "turn the dropped card to match the destination stack's facing" in:
      // deck#0 lies face-down; the discard it lands on is a face-up pile, so the
      // card inherits the stack's facing as it merges in.
      val state   = Engine.setup(catalog, rulebook, setup)
      val dropped = Engine.drop(state, CardId("deck#0"), discard).toOption.get
      stackOf(dropped, discard).cards.head.facing shouldBe Facing.Up

    "do nothing beyond the move when the arrival triggers no rule" in:
      val state   = Engine.setup(playCatalog, rulebook, playSetup)
      val dropped = Engine.drop(state, CardId("hand#0"), StackId("discard")).toOption.get
      stackOf(dropped, StackId("discard")).cards.map(_.id) shouldBe List(CardId("hand#0"))
      stackOf(dropped, StackId("build-zone")).cards shouldBe empty

  "Engine.dropSteps" should:

    "script the move first, then each effect move and reveal the cascade produces" in:
      Engine.dropSteps(Engine.setup(playCatalog, rulebook, playSetup), CardId("hand#0"), StackId("in-play")) shouldBe Right(
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

  "Engine.drop with a shuffle effect" should:

    "reorder the targeted stack while keeping its exact cards" in:
      val before = Engine.setup(playCatalog, shuffleRulebook, playSetup)
      val after  = Engine.drop(before, CardId("hand#0"), StackId("in-play"), 42L).toOption.get
      stackOf(after, StackId("debt")).cards.toSet shouldBe stackOf(before, StackId("debt")).cards.toSet

    "actually shake up the order rather than leave it as it lay" in:
      val before = Engine.setup(playCatalog, shuffleRulebook, playSetup)
      val after  = Engine.drop(before, CardId("hand#0"), StackId("in-play"), 42L).toOption.get
      stackOf(after, StackId("debt")).cards should not be stackOf(before, StackId("debt")).cards

    "flag the reordered stack as freshly shuffled" in:
      val after = Engine.drop(Engine.setup(playCatalog, shuffleRulebook, playSetup), CardId("hand#0"), StackId("in-play"), 42L).toOption.get
      stackOf(after, StackId("debt")).shuffled shouldBe true

    "reproduce the same table for the same seed" in:
      val state = Engine.setup(playCatalog, shuffleRulebook, playSetup)
      Engine.drop(state, CardId("hand#0"), StackId("in-play"), 42L) shouldBe Engine.drop(state, CardId("hand#0"), StackId("in-play"), 42L)

    "script a single shuffle step naming the targeted stack" in:
      val steps = Engine.dropSteps(Engine.setup(playCatalog, shuffleRulebook, playSetup), CardId("hand#0"), StackId("in-play"), 42L).toOption.get
      steps.collect { case Step.Shuffle(stack, _) => stack } shouldBe List(StackId("debt"))

  "Engine.dealSteps" should:

    "script one move per card off the top of the source onto the target, each revealed by the face-up target" in:
      // The deck is face-down and the discard face-up, so every dealt card flips to
      // match its destination as it lands.
      Engine.dealSteps(Engine.setup(catalog, rulebook, setup), deck, discard, 2) shouldBe Right(
        List(
          Step.Move(CardId("deck#0"), discard),
          Step.Flip(CardId("deck#0")),
          Step.Move(CardId("deck#1"), discard),
          Step.Flip(CardId("deck#1")),
        ),
      )

    "deal only as many as the source holds when asked for more" in:
      // The 12-card deck runs dry on the 12th draw; asking for 13 deals all 12 it
      // has rather than failing.
      val moves = Engine.dealSteps(Engine.setup(catalog, rulebook, setup), deck, discard, 13).map(_.collect { case m: Step.Move => m }.length)
      moves shouldBe Right(12)

  "Engine.step over a manual effect" should:

    // The cascade surfaces the drop's own move first; the manual prompt is the next
    // advance through the queue that move queued.
    def pauseOf(state: GameState): Progress =
      val Progress.Ran(moved, _, rest) =
        Engine.dropCascade(state, CardId("hand#0"), StackId("in-play")).toOption.get: @unchecked
      Engine.step(moved, rest, 0L).toOption.get

    "pause on the manual step, naming the triggering card and its prompt" in:
      val Progress.Await(_, card, description, _) =
        pauseOf(Engine.setup(playCatalog, manualRulebook, playSetup)): @unchecked
      card shouldBe CardId("hand#0")
      description shouldBe manualPrompt

    "hold the later effect back until the pause is resolved" in:
      val Progress.Await(paused, _, _, _) =
        pauseOf(Engine.setup(playCatalog, manualRulebook, playSetup)): @unchecked
      stackOf(paused, StackId("build-zone")).cards shouldBe empty

    "resume the rest against the table the player leaves behind" in:
      // The manual prompt fires before the deal, so the player can rearrange first:
      // here they pull features#0 out by hand, and the resumed deal takes the new top.
      val Progress.Await(paused, _, _, rest) =
        pauseOf(Engine.setup(playCatalog, manualRulebook, playSetup)): @unchecked
      val edited                    = Engine.move(paused, CardId("features#0"), StackId("discard")).toOption.get
      val Progress.Ran(done, _, _)  = Engine.step(edited, rest, 0L).toOption.get: @unchecked
      stackOf(done, StackId("build-zone")).cards.map(_.id) shouldBe List(CardId("features#1"))

  "Engine.dropSteps over a manual effect" should:

    "elide the prompt, splicing the batches either side of it into one script" in:
      Engine.dropSteps(Engine.setup(playCatalog, manualRulebook, playSetup), CardId("hand#0"), StackId("in-play")) shouldBe Right(
        List(
          Step.Move(CardId("hand#0"), StackId("in-play")),
          Step.Move(CardId("features#0"), StackId("build-zone")),
          Step.Flip(CardId("features#0")),
        ),
      )

  "Engine.drop over a manual effect" should:

    "apply the whole cascade headlessly, the manual step a no-op pass-through" in:
      val played = Engine.drop(Engine.setup(playCatalog, manualRulebook, playSetup), CardId("hand#0"), StackId("in-play")).toOption.get
      stackOf(played, StackId("build-zone")).cards.size shouldBe 1
      stackOf(played, StackId("features")).cards.size shouldBe 2

  "A player-relative effect (one rule for every player)" should:

    "deal to the current player's deck when player 1 is active" in:
      val state  = Engine.setup(ownerCatalog, ownerRulebook, ownerSetup)
      val played = Engine.drop(state, CardId("hand#0"), StackId("play")).toOption.get
      stackOf(played, StackId("deck1")).cards.size shouldBe 2
      stackOf(played, StackId("deck2")).cards shouldBe empty

    "retarget the same rule to player 2's deck after the turn passes" in:
      val state  = Engine.endTurn(Engine.setup(ownerCatalog, ownerRulebook, ownerSetup, players = 2))
      val played = Engine.drop(state, CardId("hand#0"), StackId("play")).toOption.get
      stackOf(played, StackId("deck2")).cards.size shouldBe 2
      stackOf(played, StackId("deck1")).cards shouldBe empty

    "abort when the current player owns no stack of the role" in:
      val state = Engine.setup(ownerCatalog, ownerRulebook, ownerSetup).copy(currentPlayer = 5)
      Engine.drop(state, CardId("hand#0"), StackId("play")) shouldBe Left(EngineError.UnresolvedRole("deck", 5))

  "A player-relative trigger" should:

    "fire when the card lands on the current player's owned stack" in:
      val state  = Engine.setup(gateCatalog, gateRulebook, gateSetup)
      val played = Engine.drop(state, CardId("hand#0"), StackId("gate1")).toOption.get
      stackOf(played, StackId("sink")).cards.size shouldBe 1

    "stay inert when the card lands on another player's owned stack" in:
      val state  = Engine.setup(gateCatalog, gateRulebook, gateSetup)
      val played = Engine.drop(state, CardId("hand#0"), StackId("gate2")).toOption.get
      stackOf(played, StackId("sink")).cards shouldBe empty

  "A button ruleset" should:

    // Drive a button cascade to its settled table, auto-resuming any manual pause —
    // the headless reading the animated shell would otherwise step through.
    def runAll(progress: Progress): GameState = progress match
      case Progress.Done(s)              => s
      case Progress.Ran(s, _, rest)      => runAll(Engine.step(s, rest, 0L).toOption.get)
      case Progress.Await(s, _, _, rest) => runAll(Engine.step(s, rest, 0L).toOption.get)

    "run a list of effects in order, resolving roles against the current player" in:
      val state   = Engine.setup(ownerCatalog, Rulebook(Nil), ownerSetup)
      val effects = List(Effect.Deal(StackRef("debt"), StackRef.Owned("deck"), 2), Effect.Shuffle(StackRef.Owned("deck")))
      val done    = runAll(Engine.buttonCascade(state, effects).toOption.get)
      stackOf(done, StackId("deck1")).cards.size shouldBe 2
      stackOf(done, StackId("deck1")).shuffled shouldBe true
      stackOf(done, StackId("deck2")).cards shouldBe empty

    "retarget the same button to the next player after the turn passes" in:
      val state = Engine.endTurn(Engine.setup(ownerCatalog, Rulebook(Nil), ownerSetup, players = 2))
      val done  = runAll(Engine.buttonCascade(state, List(Effect.Deal(StackRef("debt"), StackRef.Owned("deck"), 2))).toOption.get)
      stackOf(done, StackId("deck2")).cards.size shouldBe 2
      stackOf(done, StackId("deck1")).cards shouldBe empty

    "deal leniently, taking only what the source holds when asked for more" in:
      val state = Engine.setup(ownerCatalog, Rulebook(Nil), ownerSetup)
      val done  = runAll(Engine.buttonCascade(state, List(Effect.Deal(StackRef("debt"), StackRef.Owned("deck"), 999))).toOption.get)
      stackOf(done, StackId("deck1")).cards.size shouldBe 10

  "JSON codecs" should:

    "round-trip a catalog unchanged" in:
      read[CardCatalog](write(catalog)) shouldBe catalog

    "round-trip a button's effects unchanged" in:
      val b = StackButton(StackId("hand"), "draw", List(Effect.Deal(StackRef.Owned("deck"), StackRef.Owned("hand"), 4), Effect.Shuffle(StackRef.Owned("deck"))))
      read[StackButton](write(b)) shouldBe b

    "read a legacy button action as a one-effect list" in:
      val legacy = """{"stackId":"hand1","label":"draw 4","action":{"$type":"DealFrom","stack":"deck1","count":4}}"""
      read[StackButton](legacy) shouldBe StackButton(StackId("hand1"), "draw 4", List(Effect.Deal(StackRef("deck1"), StackRef("hand1"), 4)))

    "round-trip a player-relative rulebook unchanged" in:
      read[Rulebook](write(ownerRulebook)) shouldBe ownerRulebook

    "read a fixed stack reference from a bare string (legacy format)" in:
      read[StackRef]("\"play\"") shouldBe StackRef.Fixed(StackId("play"))

    "round-trip an owned stack reference unchanged" in:
      read[StackRef](write(StackRef.Owned("deck"))) shouldBe StackRef.Owned("deck")

    "round-trip an EndTurn effect unchanged" in:
      read[Effect](write(Effect.EndTurn)) shouldBe Effect.EndTurn

    "round-trip a setup unchanged" in:
      read[GameSetup](write(setup)) shouldBe setup

    "round-trip a rulebook of card effects unchanged" in:
      read[Rulebook](write(rulebook)) shouldBe rulebook

    "round-trip a rulebook with a shuffle effect unchanged" in:
      read[Rulebook](write(shuffleRulebook)) shouldBe shuffleRulebook

    "round-trip a rulebook with a manual effect unchanged" in:
      read[Rulebook](write(manualRulebook)) shouldBe manualRulebook

    "round-trip a setup with persistent stacks unchanged" in:
      read[GameSetup](write(playSetup)) shouldBe playSetup

    "round-trip a whole game definition unchanged" in:
      val definition = GameDefinition("test", "Test Game", catalog, rulebook, setup)
      read[GameDefinition](write(definition)) shouldBe definition
