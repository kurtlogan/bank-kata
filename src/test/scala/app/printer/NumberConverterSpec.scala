package app.printer

import app._

class NumberConverterSpec extends UnitSpec with NumbersGenerator {
  "convert" should {
    "return a list of verified account numbers" in {
      forAll(ambiguousAccountNumbers) {
        case (n, alt) ⇒
          NumberConverter.convert(n) shouldBe alt
      }
    }
  }

  "alternativeAccountNumbers" should {
    val unk = Unknown("??????????")

    "numbers are unknown" in {
      forAll(unknownGen) {
        case (unknown, actual) ⇒
          val acc =
            AccountNumber(unknown, unk, unk, unk, unk, unk, unk, unk, unk)

          NumberConverter.alternativeAccountNumbers(acc) shouldBe List(
            acc.copy(n1 = actual)
          )
      }
    }

    "list all possible values for first number" in {
      forAll(knownNumberGen) { n ⇒
        val acc =
          AccountNumber(n, unk, unk, unk, unk, unk, unk, unk, unk)

        NumberConverter.alternativeAccountNumbers(acc) shouldBe n.alternatives
          .map(alt ⇒ acc.copy(n1 = alt))
      }
    }

    "list all possible values for second number" in {
      forAll(knownNumberGen) { n ⇒
        val acc =
          AccountNumber(unk, n, unk, unk, unk, unk, unk, unk, unk)

        NumberConverter.alternativeAccountNumbers(acc) shouldBe n.alternatives
          .map(alt ⇒ acc.copy(n2 = alt))
      }
    }

    "list all possible values for third number" in {
      forAll(knownNumberGen) { n ⇒
        val acc =
          AccountNumber(unk, unk, n, unk, unk, unk, unk, unk, unk)

        NumberConverter.alternativeAccountNumbers(acc) shouldBe n.alternatives
          .map(alt ⇒ acc.copy(n3 = alt))
      }
    }

    "list all possible values for forth number" in {
      forAll(knownNumberGen) { n ⇒
        val acc =
          AccountNumber(unk, unk, unk, n, unk, unk, unk, unk, unk)

        NumberConverter.alternativeAccountNumbers(acc) shouldBe n.alternatives
          .map(alt ⇒ acc.copy(n4 = alt))
      }
    }

    "list all possible values for fifth number" in {
      forAll(knownNumberGen) { n ⇒
        val acc =
          AccountNumber(unk, unk, unk, unk, n, unk, unk, unk, unk)

        NumberConverter.alternativeAccountNumbers(acc) shouldBe n.alternatives
          .map(alt ⇒ acc.copy(n5 = alt))
      }
    }

    "list all possible values for sixth number" in {
      forAll(knownNumberGen) { n ⇒
        val acc =
          AccountNumber(unk, unk, unk, unk, unk, n, unk, unk, unk)

        NumberConverter.alternativeAccountNumbers(acc) shouldBe n.alternatives
          .map(alt ⇒ acc.copy(n6 = alt))
      }
    }

    "list all possible values for seventh number" in {
      forAll(knownNumberGen) { n ⇒
        val acc =
          AccountNumber(unk, unk, unk, unk, unk, unk, n, unk, unk)

        NumberConverter.alternativeAccountNumbers(acc) shouldBe n.alternatives
          .map(alt ⇒ acc.copy(n7 = alt))
      }
    }

    "list all possible values for eighth number" in {
      forAll(knownNumberGen) { n ⇒
        val acc =
          AccountNumber(unk, unk, unk, unk, unk, unk, unk, n, unk)

        NumberConverter.alternativeAccountNumbers(acc) shouldBe n.alternatives
          .map(alt ⇒ acc.copy(n8 = alt))
      }
    }

    "list all possible values for ninth number" in {
      forAll(knownNumberGen) { n ⇒
        val acc =
          AccountNumber(unk, unk, unk, unk, unk, unk, unk, unk, n)

        NumberConverter.alternativeAccountNumbers(acc) shouldBe n.alternatives
          .map(alt ⇒ acc.copy(n9 = alt))
      }
    }
  }
}
