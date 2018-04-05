package app

import app.parser.AccountNumberParser
import app.printer.AccountNumberPrinter

object Main {
  def process(s: String): String =
    AccountNumberParser.parse(s) match {
      case Right(as) ⇒ AccountNumberPrinter.print(as)
      case Left(msg) ⇒ msg
    }
}
