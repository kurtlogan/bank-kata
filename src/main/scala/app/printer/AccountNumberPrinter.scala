package app.printer

import app._
import app.verification.{ ChecksumError, IllegibleError }

object AccountNumberPrinter {
  import app.verification.AccountNumberVerification.verify

  def print(as: List[AccountNumber]): String = as.map(printLine).mkString("\n")

  def printLine(a: AccountNumber): String =
    verify(a) match {
      case Right(a)                ⇒ accountNumberAsString(a)
      case Left(ChecksumError(a))  ⇒ s"${accountNumberAsString(a)} ERR"
      case Left(IllegibleError(a)) ⇒ s"${accountNumberAsString(a)} ILL"
    }

  private def accountNumberAsString(acc: AccountNumber): String =
    AccountNumber.asList(acc).foldLeft("")(_ + numberAsString(_))

  private def numberAsString(n: Number): String =
    Number.asInt(n).map(_.toString).getOrElse("?")
}
