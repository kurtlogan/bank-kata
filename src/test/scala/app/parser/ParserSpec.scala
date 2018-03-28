package app.parser

import app.{ NumbersGenerator, UnitSpec }
import org.scalatest.prop.GeneratorDrivenPropertyChecks.forAll

class ParserSpec extends UnitSpec with NumbersGenerator {

  "parseLine" should {
    "return number from string" in {
      forAll(singleNumberGen) { number ⇒
        //number.digital shouldBe number
      }
    }
  }

}
