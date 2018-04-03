package app.printer

import app._

class AccountNumberPrinterSpec extends UnitSpec with NumbersGenerator {
  "print" should {
    "print account numbers" when {
      "account numbers are valid" in {
        forAll(validAccountNumber) { n ⇒
          AccountNumberPrinter.print(List(n)) shouldBe accountNumberAsString(n)
        }
      }

      "account numbers fail checksum" in {
        forAll(invalidAccountNumber) { n ⇒
          AccountNumberPrinter.print(List(n)) shouldBe s"${accountNumberAsString(n)} ERR"
        }
      }

      "account numbers are illegible" in {
        forAll(illegibleAccountNumbers) { n ⇒
          AccountNumberPrinter.print(List(n)) shouldBe s"${accountNumberAsString(n)} ILL"
        }
      }

      "multiple account numbers are provided" in {
        forAll(validAccountNumberListGen) { ns ⇒
          AccountNumberPrinter.print(ns) shouldBe ns
            .map(accountNumberAsString)
            .mkString("\n")
        }
      }
    }
  }

  private def accountNumberAsString(acc: AccountNumber): String =
    acc.foldLeft("")(_ + numberAsString(_))
  private def numberAsString(n: Number): String =
    n.toMaybeInt.map(_.toString).getOrElse("?")
}
