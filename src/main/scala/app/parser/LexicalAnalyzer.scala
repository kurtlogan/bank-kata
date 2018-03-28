package app.parser

import scala.util.parsing.combinator.RegexParsers

object LexicalAnalyzer extends RegexParsers {
  // Whitespace is significant - do not skip
  override val skipWhitespace: Boolean = false

  val number = "[\\s_\\|]{9}".r ^^ { x ⇒
    DigitalNumber(x)
  }

  val continuation = "[\\n]".r ^^ { _ ⇒
    Continuation
  }

  val stop = "[\\n]{2}".r ^^ { _ ⇒
    Stop
  }

  val tokens = phrase(
    rep1(number) ^^ { x ⇒
      x
    }
  )

  def analyze(input: String): Either[String, List[Tokens]] = {
    parse(tokens, input) match {
      case NoSuccess(msg, _)  ⇒ Left("")
      case Success(result, _) ⇒ Right(result)
    }
  }
}
