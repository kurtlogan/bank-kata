package app.typeclasses

import app._
import org.scalatest.prop.GeneratorDrivenPropertyChecks.forAll

class DigitalFormatSpec extends UnitSpec {
  import DigitalFormat._

  // format: off

  "Zero.digital" should {
    "return expected digital number" in {
      Zero().digital shouldBe
        " _ " +
        "| |" +
        "|_|"
    }
  }

  "One.digital" should {
    "return 1 as a digital number" in {
      One().digital shouldBe
        "   " +
        "  |" +
        "  |"
    }
  }

  "Two.digital" should {
    "return 2 as a digital number" in {
      Two().digital shouldBe
        " _ " +
        " _|" +
        "|_ "
    }
  }

  "Three.digital" should {
    "return 3 as a digital number" in {
      Three().digital shouldBe
        " _ " +
        " _|" +
        " _|"
    }
  }

  "Four.digital" should {
    "return 4 as a digital number" in {
      Four().digital shouldBe
        "   " +
        "|_|" +
        "  |"
    }
  }

  "Five.digital" should {
    "return 5 as a digital number" in {
      Five().digital shouldBe
        " _ " +
        "|_ " +
        " _|"
    }
  }

  "Six.digital" should {
    "return 6 as a digital number" in {
      Six().digital shouldBe
        " _ " +
        "|_ " +
        "|_|"

    }
  }

  "Seven.digital" should {
    "return 7 as a digital number" in {
      Seven().digital shouldBe
        " _ " +
        "  |" +
        "  |"
    }
  }

  "Eight.digital" should {
    "return 8 as a digital number" in {
      Eight().digital shouldBe
        " _ " +
        "|_|" +
        "|_|"
    }
  }

  "Nine.digital" should {
    "return 9 as a digital number" in {
      Nine().digital shouldBe
        " _ " +
        "|_|" +
        " _|"
    }
  }

  "Unknown.digital" should {
    "return unknown value" in {
      forAll { (s: String) ⇒
        Unknown(s).digital shouldBe s
      }
    }
  }

  // format: on
}
