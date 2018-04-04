package app.printer

import app._

class NumberConverterSpec extends UnitSpec with NumbersGenerator {
  "convert" should {
    "return a list of verified account numbers" when {
      "all numbers are known" in {
        forAll(ambiguousAccountNumbers) {
          case (n, alt) ⇒
            NumberConverter.convert(n).sortBy(_.toString) shouldBe alt.sortBy(
              _.toString
            )
        }
      }
    }
  }

  "combinationList" should {
    "list all possible values for account number" in {
      val acc = AccountNumber(
        One(),
        One(),
        One(),
        One(),
        One(),
        One(),
        One(),
        One(),
        One()
      )
      NumberConverter.alternativeAccountNumbers(acc) shouldBe
        List(
          AccountNumber(
            Seven(),
            One(),
            One(),
            One(),
            One(),
            One(),
            One(),
            One(),
            One()
          ),
          AccountNumber(
            One(),
            Seven(),
            One(),
            One(),
            One(),
            One(),
            One(),
            One(),
            One()
          ),
          AccountNumber(
            One(),
            One(),
            Seven(),
            One(),
            One(),
            One(),
            One(),
            One(),
            One()
          ),
          AccountNumber(
            One(),
            One(),
            One(),
            Seven(),
            One(),
            One(),
            One(),
            One(),
            One()
          ),
          AccountNumber(
            One(),
            One(),
            One(),
            One(),
            Seven(),
            One(),
            One(),
            One(),
            One()
          ),
          AccountNumber(
            One(),
            One(),
            One(),
            One(),
            One(),
            Seven(),
            One(),
            One(),
            One()
          ),
          AccountNumber(
            One(),
            One(),
            One(),
            One(),
            One(),
            One(),
            Seven(),
            One(),
            One()
          ),
          AccountNumber(
            One(),
            One(),
            One(),
            One(),
            One(),
            One(),
            One(),
            Seven(),
            One()
          ),
          AccountNumber(
            One(),
            One(),
            One(),
            One(),
            One(),
            One(),
            One(),
            One(),
            Seven()
          )
        )
    }
  }
}
