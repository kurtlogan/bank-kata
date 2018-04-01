package app

class NumberSpec extends UnitSpec {

  "asInt" should {
    import Number.asInt

    "return for Zero" in {
      asInt(Zero()) shouldBe Some(0)
    }

    "return for One" in {
      asInt(One()) shouldBe Some(1)
    }

    "return for Two" in {
      asInt(Two()) shouldBe Some(2)
    }

    "return for Three" in {
      asInt(Three()) shouldBe Some(3)
    }

    "return for Four" in {
      asInt(Four()) shouldBe Some(4)
    }

    "return for Five" in {
      asInt(Five()) shouldBe Some(5)
    }

    "return for Six" in {
      asInt(Six()) shouldBe Some(6)
    }

    "return for Seven" in {
      asInt(Seven()) shouldBe Some(7)
    }

    "return for Eight" in {
      asInt(Eight()) shouldBe Some(8)
    }

    "return for Nine" in {
      asInt(Nine()) shouldBe Some(9)
    }

    "return for Unknown" in {
      asInt(Unknown("")) shouldBe None
    }
  }

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
