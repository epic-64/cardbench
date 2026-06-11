package engine

import upickle.default.{ReadWriter, readwriter}

// Two authored artifacts. JSON is the on-disk format (upickle); these case
// classes are the schema.

/** How a stack's authored cards are arranged at setup. `Ordered` keeps them as
  * written; `Shuffled` deals them in a seeded-random order.
  */
enum Arrangement:
  case Ordered, Shuffled

object Arrangement:
  // Lowercase on the wire, matching the authored setup JSON.
  given ReadWriter[Arrangement] = readwriter[String].bimap(
    {
      case Arrangement.Ordered  => "ordered"
      case Arrangement.Shuffled => "shuffled"
    },
    {
      case "ordered"  => Arrangement.Ordered
      case "shuffled" => Arrangement.Shuffled
      case other      => sys.error(s"Unknown arrangement: $other")
    },
  )

/** How many instances of a definition to spawn into a stack. */
case class SpawnSpec(card: CardDefId, count: Int) derives ReadWriter

/** A starting pile: where it sits, how its cards face, and what fills it. */
case class StackSpec(
  id: StackId,
  label: String,
  position: Position,
  facing: Facing,             // applied to every card spawned here
  contents: List[SpawnSpec],                      // expanded into instances, in order
  arrangement: Arrangement = Arrangement.Ordered, // how the spawned cards are arranged at setup
  persistent: Boolean = false,                    // an essential pile: kept on the table even at 0 cards
  layout: Layout = Layout.Pile,                   // how the stack is shown: a heap (Pile) or a row of cards
) derives ReadWriter

/** The starting table. */
case class GameSetup(stacks: List[StackSpec]) derives ReadWriter

/** What cards exist, for rendering. */
case class CardCatalog(cards: List[CardDef]) derives ReadWriter
