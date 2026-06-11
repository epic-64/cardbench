package engine

import upickle.default.ReadWriter

// Two authored artifacts. JSON is the on-disk format (upickle); these case
// classes are the schema.

/** How many instances of a definition to spawn into a stack. */
case class SpawnSpec(card: CardDefId, count: Int) derives ReadWriter

/** A starting pile: where it sits, how its cards face, and what fills it. */
case class StackSpec(
  id: StackId,
  label: String,
  position: Position,
  facing: Facing,            // applied to every card spawned here
  contents: List[SpawnSpec], // expanded into instances, in order
) derives ReadWriter

/** The starting table. */
case class GameSetup(stacks: List[StackSpec]) derives ReadWriter

/** What cards exist, for rendering. */
case class CardCatalog(cards: List[CardDef]) derives ReadWriter
