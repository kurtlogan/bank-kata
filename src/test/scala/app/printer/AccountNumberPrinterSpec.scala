package app.printer

import app._

class AccountNumberPrinterSpec extends UnitSpec with NumbersGenerator {
  "print" should {
    "print account numbers" when {
      "account numbers are valid" in {
        forAll(validAccountNumber) { n ⇒
          AccountNumberPrinter.print(List(n)) shouldBe n.toPrintableString
        }
      }

      "account numbers fail checksum and no alternative available" in {
        forAll(invalidAccountNumber) { n ⇒
          AccountNumberPrinter.print(List(n)) shouldBe s"${n.toPrintableString} ILL"
        }
      }

      "account numbers are illegible" in {
        forAll(illegibleAccountNumbers) { n ⇒
          AccountNumberPrinter.print(List(n)) shouldBe s"${n.toPrintableString} ILL"
        }
      }

      "multiple account numbers are provided" in {
        forAll(validAccountNumberListGen) { ns ⇒
          AccountNumberPrinter.print(ns) shouldBe ns
            .map(_.toPrintableString)
            .mkString("\n")
        }
      }

      "accounts numbers where one digit is wrong" in {
        forAll(ambiguousAccountNumbers) {
          case (n, alts) ⇒
            val altsString =
              alts.map(n ⇒ s"'${n.toPrintableString}'").mkString(", ")

            AccountNumberPrinter.print(List(n)) shouldBe s"${n.toPrintableString} AMB [$altsString]"
        }
      }

      "one number is off a character" in {
        forAll(offByOneNumberGen) {
          case (unknown, actual) ⇒
            AccountNumberPrinter.print(List(unknown)) shouldBe actual.toPrintableString
        }
      }
    }
  }
}
