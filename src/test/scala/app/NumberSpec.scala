package app

import org.scalatest.prop.GeneratorDrivenPropertyChecks.forAll

class NumberSpec extends UnitSpec {
  // format: off

  "asString" should {
    import Number.asString

    "return for Zero" in {
      asString(Zero()) shouldBe
        " _ " +
        "| |" +
        "|_|"
    }

    "return for One" in {
      asString(One()) shouldBe
        "   " +
        "  |" +
        "  |"
    }

    "return for Two" in {
      asString(Two()) shouldBe
        " _ " +
        " _|" +
        "|_ "
    }

    "return for Three" in {
      asString(Three()) shouldBe
        " _ " +
        " _|" +
        " _|"
    }

    "return for Four" in {
      asString(Four()) shouldBe
        "   " +
        "|_|" +
        "  |"
    }

    "return for Five" in {
      asString(Five()) shouldBe
        " _ " +
        "|_ " +
        " _|"
    }

    "return for Six" in {
      asString(Six()) shouldBe
        " _ " +
        "|_ " +
        "|_|"
    }

    "return for Seven" in {
      asString(Seven()) shouldBe
        " _ " +
        "  |" +
        "  |"
    }

    "return for Eight" in {
      asString(Eight()) shouldBe
        " _ " +
        "|_|" +
        "|_|"
    }

    "return for Nine" in {
      asString(Nine()) shouldBe
        " _ " +
        "|_|" +
        " _|"
    }

    "return for Unknown" in {
      forAll { (s: String) ⇒
        asString(Unknown(s)) shouldBe s
      }
    }
  }

  // format: on
}
