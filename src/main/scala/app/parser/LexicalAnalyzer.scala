package app.parser

import scala.util.parsing.combinator.RegexParsers

object LexicalAnalyzer extends RegexParsers {
  val number = "(\\s_|){9}".r ^^ { DigitalNumber }

  val continuation = "\\n".r ^^ { _ ⇒
    Continuation
  }

  val stop = "\\n{2}".r ^^ { _ ⇒
    Stop
  }

  val tokens = phrase(
    rep1(
      number ^^ { x ⇒
        x
      }
    )
  )

  def analyze(input: String): ParseResult[List[DigitalNumber]] = {
    parse(tokens, input)
  }
}
