package app.parser

import app.{ Number, NumbersGenerator, UnitSpec }

class AccountNumberParserSpec extends UnitSpec with NumbersGenerator {
  implicit def numberToString(n: app.Number): String = app.Number.asString(n)

  private def accountNumbersAsString(xs: List[AccountNumber]): String =
    xs match {
      case List()         ⇒ ""
      case head :: List() ⇒ accountNumberAsString(head)
      case head :: tail ⇒
        accountNumberAsString(head) + "\n" + accountNumbersAsString(tail)
    }

  private def accountNumberAsString(a: AccountNumber): String =
    s"${Number.asString(a.n1)}${Number.asString(a.n2)}${Number.asString(a.n3)}\n" +
      s"${Number.asString(a.n4)}${Number.asString(a.n5)}${Number.asString(a.n6)}\n" +
      s"${Number.asString(a.n7)}${Number.asString(a.n8)}${Number.asString(a.n9)}\n" +
      (" " * 27)

  "parse" should {
    "parse account number" in {
      forAll(accountNumberListGen) { list ⇒
        AccountNumberParser
          .analyze(accountNumbersAsString(list))
          .merge shouldBe list
      }
    }

    "fail with invalid input" in {
      forAll { s: String ⇒
        whenever(!s.isEmpty) {
          AccountNumberParser.analyze(s).isLeft shouldBe true
        }
      }
    }
  }
}
