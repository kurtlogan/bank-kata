package app

class NumberSpec extends UnitSpec {

  "asInt" should {

    "return for Zero" in {
      Zero().toMaybeInt shouldBe Some(0)
    }

    "return for One" in {
      One().toMaybeInt shouldBe Some(1)
    }

    "return for Two" in {
      Two().toMaybeInt shouldBe Some(2)
    }

    "return for Three" in {
      Three().toMaybeInt shouldBe Some(3)
    }

    "return for Four" in {
      Four().toMaybeInt shouldBe Some(4)
    }

    "return for Five" in {
      Five().toMaybeInt shouldBe Some(5)
    }

    "return for Six" in {
      Six().toMaybeInt shouldBe Some(6)
    }

    "return for Seven" in {
      Seven().toMaybeInt shouldBe Some(7)
    }

    "return for Eight" in {
      Eight().toMaybeInt shouldBe Some(8)
    }

    "return for Nine" in {
      Nine().toMaybeInt shouldBe Some(9)
    }

    "return for Unknown" in {
      Unknown("").toMaybeInt shouldBe None
    }
  }

  // format: off

  "asString" should {

    "return for Zero" in {
      Zero().toDigitalString shouldBe
        " _ " +
        "| |" +
        "|_|"
    }

    "return for One" in {
      One().toDigitalString shouldBe
        "   " +
        "  |" +
        "  |"
    }

    "return for Two" in {
      Two().toDigitalString shouldBe
        " _ " +
        " _|" +
        "|_ "
    }

    "return for Three" in {
      Three().toDigitalString shouldBe
        " _ " +
        " _|" +
        " _|"
    }

    "return for Four" in {
      Four().toDigitalString shouldBe
        "   " +
        "|_|" +
        "  |"
    }

    "return for Five" in {
      Five().toDigitalString shouldBe
        " _ " +
        "|_ " +
        " _|"
    }

    "return for Six" in {
      Six().toDigitalString shouldBe
        " _ " +
        "|_ " +
        "|_|"
    }

    "return for Seven" in {
      Seven().toDigitalString shouldBe
        " _ " +
        "  |" +
        "  |"
    }

    "return for Eight" in {
      Eight().toDigitalString shouldBe
        " _ " +
        "|_|" +
        "|_|"
    }

    "return for Nine" in {
      Nine().toDigitalString shouldBe
        " _ " +
        "|_|" +
        " _|"
    }

    "return for Unknown" in {
      forAll { (s: String) ⇒
        Unknown(s).toDigitalString shouldBe s
      }
    }
  }

  // format: on
}
