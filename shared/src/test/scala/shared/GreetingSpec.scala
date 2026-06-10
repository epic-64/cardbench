package shared

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class GreetingSpec extends AnyWordSpec with Matchers:

  "Greeting.message" should:
    "be the hello world greeting" in:
      Greeting.message shouldBe "Hello, World!"
