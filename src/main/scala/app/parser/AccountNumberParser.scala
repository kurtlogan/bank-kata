package app.parser

import app._

import scala.util.parsing.combinator.RegexParsers

object AccountNumberParser extends RegexParsers {
  // Whitespace is significant - do not skip
  override val skipWhitespace: Boolean = false

  val number: Parser[Number] = "[\\s_\\|]{9}".r ^^ {
    case Zero(_)  ⇒ Zero()
    case One(_)   ⇒ One()
    case Two(_)   ⇒ Two()
    case Three(_) ⇒ Three()
    case Four(_)  ⇒ Four()
    case Five(_)  ⇒ Five()
    case Six(_)   ⇒ Six()
    case Seven(_) ⇒ Seven()
    case Eight(_) ⇒ Eight()
    case Nine(_)  ⇒ Nine()
    case n        ⇒ Unknown(n)
  }

  val newline = "\\n".r ^^ { _ ⇒
    NewLine
  }

  val numbers: Parser[(Number, Number, Number)] =
    number ~ number ~ number ~ newline ^^ {
      case n1 ~ n2 ~ n3 ~ _ ⇒ (n1, n2, n3)
    }

  val accountNumber: Parser[AccountNumber] = numbers ~ numbers ~ numbers ^^ {
    case (n1, n2, n3) ~ ((n4, n5, n6)) ~ ((n7, n8, n9)) ⇒
      AccountNumber(n1, n2, n3, n4, n5, n6, n7, n8, n9)
  }

  val accountNumberWithNewLine
    : Parser[AccountNumber] = accountNumber ~ newline ^^ { case acc ~ _ ⇒ acc }

  val tokens: Parser[List[AccountNumber]] = phrase(
    rep(accountNumberWithNewLine | accountNumber) ^^ { x ⇒
      x
    }
  )

  def analyze(input: String): Either[String, List[AccountNumber]] = {
    parse(tokens, input) match {
      case NoSuccess(msg, _)  ⇒ Left(msg)
      case Success(result, _) ⇒ Right(result)
    }
  }
}
