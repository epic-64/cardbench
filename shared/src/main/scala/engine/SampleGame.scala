package engine

/** A hand-authored catalog and table so the client has something to shuffle and
  * deal. Pure data — the engine knows nothing about what these cards mean.
  */
object SampleGame:

  val catalog: CardCatalog = CardCatalog(
    List(
      CardDef(CardDefId("build"), "#3b82f6", "Build", "Ship a feature to fulfil a Request."),
      CardDef(CardDefId("refactor"), "#22c55e", "Refactor", "Trash 1 Debt card."),
      CardDef(CardDefId("test"), "#eab308", "Test", "Prevent the next Bug / Audit event."),
      CardDef(CardDefId("backup"), "#a855f7", "Backup", "Prevent the next data-loss disaster."),
      CardDef(CardDefId("debt"), "#6b7280", "Tech Debt", "Dead weight. Clogs your hand."),
    ),
  )

  // No empty stacks: a stack exists only while it holds cards. You make new
  // piles mid-game by dragging a card onto empty board space.
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
        ),
      ),
      StackSpec(
        StackId("tools"),
        "Tools",
        Position(260, 40),
        Facing.Up,
        List(
          SpawnSpec(CardDefId("test"), 2),
          SpawnSpec(CardDefId("backup"), 2),
        ),
      ),
    ),
  )
