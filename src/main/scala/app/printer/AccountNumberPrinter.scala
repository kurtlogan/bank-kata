package app.printer

import app._
import app.verification.{ ChecksumError, IllegibleError }

object AccountNumberPrinter {
  import app.verification.AccountNumberVerification.verify
  import NumberConverter.convert

  def print(as: List[AccountNumber]): String = as.map(printLine).mkString("\n")

  def printLine(a: AccountNumber): String =
    verify(a) match {
      case Right(a)                ⇒ a.toPrintableString
      case Left(ChecksumError(a))  ⇒ error(a)
      case Left(IllegibleError(a)) ⇒ error(a)
    }

  private def error(a: AccountNumber): String = {
    convert(a) match {
      case List()         ⇒ ill(a)
      case head :: List() ⇒ head.toPrintableString
      case as             ⇒ amb(a, as)
    }
  }

  private def accountNumbersAsString(as: List[AccountNumber]): String =
    as match {
      case List()      ⇒ ""
      case h :: List() ⇒ s"'${h.toPrintableString}'"
      case h :: t ⇒
        s"'${h.toPrintableString}', ${accountNumbersAsString(t)}"
    }

  private def amb(a: AccountNumber, as: List[AccountNumber]) =
    s"${a.toPrintableString} AMB [${accountNumbersAsString(as)}]"
  private def ill(a: AccountNumber) = s"${a.toPrintableString} ILL"
}
