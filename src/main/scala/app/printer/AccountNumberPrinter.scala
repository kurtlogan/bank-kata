package app.printer

import app._
import app.verification.{ ChecksumError, IllegibleError }

object AccountNumberPrinter {
  import app.verification.AccountNumberVerification.verify
  import NumberConverter.convert

  def print(as: List[AccountNumber]): String = as.map(printLine).mkString("\n")

  def printLine(a: AccountNumber): String =
    verify(a) match {
      case Right(a) ⇒ a.asString

      case Left(ChecksumError(a)) ⇒
        convert(a) match {
          case List() ⇒ err(a)
          case as     ⇒ amb(a, as)

        }

      case Left(IllegibleError(a)) ⇒ ill(a)
    }

  private def accountNumbersAsString(as: List[AccountNumber]): String =
    as match {
      case List()      ⇒ ""
      case h :: List() ⇒ s"'${h.asString}'"
      case h :: t ⇒
        s"'${h.asString}', ${accountNumbersAsString(t)}"
    }

  private def err(a: AccountNumber) = s"${a.asString} ERR"
  private def amb(a: AccountNumber, as: List[AccountNumber]) =
    s"${a.asString} AMB [${accountNumbersAsString(as)}]"
  private def ill(a: AccountNumber) = s"${a.asString} ILL"
}
