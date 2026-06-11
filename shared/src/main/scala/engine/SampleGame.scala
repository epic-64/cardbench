package engine

/** A hand-authored catalog and table so the client has something to shuffle and
  * deal. Pure data — the engine knows nothing about what these cards mean.
  */
object SampleGame:

  /** The pile that resolves a card when you drop one onto it. Dragging a card
    * here plays it (its effects fire) and sends it to its `playsTo` destination.
    */
  val playZone: StackId = StackId("play")

  val catalog: CardCatalog = CardCatalog(
    List(
      // HAND CARDS
      CardDef(
        CardDefId("build"),
        "#3b82f6",
        "Build",
        "Ship a feature to fulfil a Request.",
        effects = List(
          // First you draw a feature into your building zone, revealed…
          Effect.Deal(StackId("features"), StackId("build-zone"), 1, reveal = true),
          // …then shipping accrues debt: two Tech Debt cards turn up in your discard.
          Effect.Deal(StackId("debt"), StackId("discard"), 2, reveal = true),
        ),
        playsTo = Some(StackId("discard")), // the spent Build card goes to discard
      ),
      CardDef(CardDefId("refactor"), "#22c55e", "Refactor", "Remove 2 debt cards from your hand or deck."),
      CardDef(CardDefId("test"), "#eab308", "Test", "Prevent the next Bug event."),
      CardDef(CardDefId("backup"), "#a855f7", "Backup", "Prevent the next data-loss disaster."),
      CardDef(CardDefId("debt"), "#6b7280", "Tech Debt", "Dead weight. Clogs your hand."),
      CardDef(CardDefId("crunch"), "#f97316", "Crunch", "FREE: add a Tech Debt to your deck, then play an additional card this turn."),
      CardDef(CardDefId("modular-design"), "#14b8a6", "Modular Design", "FREE: move one feature to a different project."),

      // PROJECT CARDS
      CardDef(CardDefId("feature-green"), "#16a34a", "Feature (Green)", "A shipped feature. Fills out a project."),
      CardDef(CardDefId("feature-blue"), "#2563eb", "Feature (Blue)", "A shipped feature. Fills out a project."),
      CardDef(CardDefId("feature-red"), "#ef4444", "Feature (Red)", "A shipped feature. Fills out a project."),

      // EVENT CARDS
      CardDef(CardDefId("bug"), "#dc2626", "Bug", "Delete one feature, unless it is protected by a Test."),
      CardDef(CardDefId("data-loss"), "#7f1d1d", "Data Loss", "For every Tech Debt in your hand, delete one feature from your largest project."),
      CardDef(CardDefId("stakeholder-pivot"), "#db2777", "Stakeholder Pivot", "Replace the middle feature of a customer project with a new card from the feature deck."),
      CardDef(CardDefId("burnout"), "#ea580c", "Burnout", "If you have two or more Tech Debt cards, discard one non-debt card from your hand."),
      CardDef(CardDefId("ci-cd"), "#0891b2", "CI/CD", "If you have 2 or more Tests in play, draw a card and play an additional card this turn."),
    ),
  )

  // Most stacks exist only while they hold cards; you make new piles mid-game by
  // dragging a card onto empty board space. The player's essential piles — deck,
  // discard, and feature-building zone — are `persistent`, so they stay on the
  // table even at zero cards and can always be targeted by a card's effects.
  val setup: GameSetup = GameSetup(
    List(
      StackSpec(
        StackId("deck"),
        "Dev Deck",
        Position(40, 40),
        Facing.Down,
        List(
          SpawnSpec(CardDefId("build"), 8),
          SpawnSpec(CardDefId("refactor"), 4),
          SpawnSpec(CardDefId("crunch"), 2),
          SpawnSpec(CardDefId("modular-design"), 2),
          SpawnSpec(CardDefId("test"), 2),
          SpawnSpec(CardDefId("backup"), 2),
        ),
        arrangement = Arrangement.Shuffled,
        persistent = true,
      ),
      StackSpec(
        StackId("debt"),
        "Tech Debt",
        Position(260, 40),
        Facing.Down,
        List(
          SpawnSpec(CardDefId("debt"), 30),
        ),
      ),
      StackSpec(
        StackId("features"),
        "Feature Deck",
        Position(480, 40),
        Facing.Down,
        List(
          SpawnSpec(CardDefId("feature-green"), 4),
          SpawnSpec(CardDefId("feature-blue"), 4),
          SpawnSpec(CardDefId("feature-red"), 4),
        ),
        arrangement = Arrangement.Shuffled,
      ),
      StackSpec(
        StackId("events"),
        "Event Deck",
        Position(700, 40),
        Facing.Down,
        List(
          SpawnSpec(CardDefId("bug"), 3),
          SpawnSpec(CardDefId("data-loss"), 2),
          SpawnSpec(CardDefId("stakeholder-pivot"), 2),
          SpawnSpec(CardDefId("burnout"), 2),
          SpawnSpec(CardDefId("ci-cd"), 2),
        ),
        arrangement = Arrangement.Shuffled,
      ),
      // Essential player piles: empty at the start, kept alive by `persistent`
      // so effects (e.g. Build's) always have somewhere to deal into.
      StackSpec(
        StackId("discard"),
        "Discard",
        Position(40, 260),
        Facing.Up,
        Nil,
        persistent = true,
      ),
      StackSpec(
        StackId("build-zone"),
        "Building",
        Position(260, 260),
        Facing.Up,
        Nil,
        persistent = true,
        layout = Layout.Row, // built features sit side by side, not in a heap
      ),
      // Drop a card here to play it: its effects resolve and it moves on to its
      // post-play destination (see CardDef.playsTo).
      StackSpec(
        playZone,
        "Play",
        Position(480, 260),
        Facing.Up,
        Nil,
        persistent = true,
      ),
    ),
  )
