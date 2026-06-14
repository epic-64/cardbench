package engine

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import upickle.default.read

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Try}

/** Backward-compatibility guard. Reads every JSON file dropped into
  * `jvm/src/test/resources/legacy-games` and asserts it still deserializes into
  * the current `GameDefinition` model. Drop exported game files there (see the
  * directory's README) to lock in their loadability across model changes.
  *
  * JVM-only because it touches the filesystem; lives under `jvm/src/test` so it
  * never compiles for the JS platform.
  */
class LegacyGameJsonSpec extends AnyWordSpec with Matchers:

  private val legacyDir: Path =
    Paths.get(getClass.getResource("/legacy-games").toURI)

  // Every dropped .json file paired with the outcome of reading it. The README
  // alongside the files (and any non-json) is ignored.
  private val loadResults: List[(String, Try[GameDefinition])] =
    Files
      .list(legacyDir)
      .iterator
      .asScala
      .toList
      .filter(_.getFileName.toString.endsWith(".json"))
      .sortBy(_.getFileName.toString)
      .map(f => f.getFileName.toString -> Try(read[GameDefinition](Files.readString(f))))

  "Every JSON file in the legacy-games directory" should {
    "deserialize into the current GameDefinition model" in {
      val failures = loadResults.collect { case (name, Failure(e)) => s"$name → ${e.getMessage}" }
      withClue(failures.mkString("legacy games failed to load:\n  ", "\n  ", "\n")):
        failures shouldBe empty
    }
  }
