package app.printer

import app._

class AccountNumberPrinterSpec extends UnitSpec with NumbersGenerator {
  "print" should {
    "print account numbers" when {
      "account numbers are valid" in {
        forAll(validAccountNumber) { n ⇒
          AccountNumberPrinter.print(List(n)) shouldBe n.asString
        }
      }

      "account numbers fail checksum" in {
        forAll(invalidAccountNumber) { n ⇒
          AccountNumberPrinter.print(List(n)) shouldBe s"${n.asString} ERR"
        }
      }

      "account numbers are illegible" in {
        forAll(illegibleAccountNumbers) { n ⇒
          AccountNumberPrinter.print(List(n)) shouldBe s"${n.asString} ILL"
        }
      }

      "multiple account numbers are provided" in {
        forAll(validAccountNumberListGen) { ns ⇒
          AccountNumberPrinter.print(ns) shouldBe ns
            .map(_.asString)
            .mkString("\n")
        }
      }

      "accounts numbers where one digit is wrong" in {
        forAll(ambiguousAccountNumbers) {
          case (n, alts) ⇒
            val altsString = alts.map(n ⇒ s"'${n.asString}'").mkString(", ")

            println(altsString)

            AccountNumberPrinter.print(List(n)) shouldBe s"${n.asString} AMB [${alts
              .map { n ⇒
                s"'${n.asString}'"
              }
              .mkString(", ")}]"
        }
      }
    }
  }

  private def accountNumberAsString(acc: AccountNumber): String =
    acc.foldLeft("")(_ + _.asString)
  private def numberAsString(n: Number): String =
    n.toMaybeInt.map(_.toString).getOrElse("?")
}
