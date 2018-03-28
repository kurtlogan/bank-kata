package app.parser

import app.{ NumbersGenerator, UnitSpec }
import org.scalatest.prop.GeneratorDrivenPropertyChecks.forAll

class LexicalAnalyzerSpec extends UnitSpec with NumbersGenerator {
  import app.Number.asString

  "analyze" should {
    "return number" when {
      "valid number is provided" in {
        forAll(singleNumberGen) { number ⇒
          LexicalAnalyzer.analyze(asString(number)).get shouldBe List(
            DigitalNumber(asString(number))
          )
        }
      }
    }
  }
}
