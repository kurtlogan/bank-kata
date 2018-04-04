package app.parser

import app.{ AccountNumber, NumbersGenerator, UnitSpec }

class AccountNumberParserSpec extends UnitSpec with NumbersGenerator {

  "parse" should {
    "parse account number" in {
      forAll(accountNumberListGen) { list ⇒
        AccountNumberParser
          .parse(accountNumbersAsString(list))
          .merge shouldBe list
      }
    }

    "fail with invalid input" in {
      forAll { s: String ⇒
        whenever(!s.isEmpty) {
          AccountNumberParser.parse(s).isLeft shouldBe true
        }
      }
    }
  }

  private def accountNumbersAsString(xs: List[AccountNumber]): String =
    xs match {
      case List()         ⇒ ""
      case head :: List() ⇒ accountNumberAsString(head)
      case head :: tail ⇒
        accountNumberAsString(head) + "\n" + accountNumbersAsString(tail)
    }

  private def accountNumberAsString(a: AccountNumber): String =
    s"${a.n1.toDigitalString}${a.n2.toDigitalString}${a.n3.toDigitalString}\n" +
      s"${a.n4.toDigitalString}${a.n5.toDigitalString}${a.n6.toDigitalString}\n" +
      s"${a.n7.toDigitalString}${a.n8.toDigitalString}${a.n9.toDigitalString}\n" +
      (" " * 27)
}
