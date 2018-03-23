import scala.annotation.tailrec

sealed trait Number {
  val digit: Int
  val analogue: String
}

case object Zero extends Number {
  override val digit: Int = 0
  override val analogue =
    " _ " +
    "| |" +
    "|_|"
}

case object One extends Number {
  override val digit = 1
  override val analogue =
    "   " +
    "  |" +
    "  |"
}

val zero = Zero.analogue
val one = One.analogue

def parse(s: String): List[Number] = s.splitAt(9) match {
    case (Zero.analogue, tail) => Zero :: parse(tail)
    case (One.analogue, tail)  => One  :: parse(tail)
    case (_, tail) => Unknown :: parse(tail)
    case _ => Nil
}

parse(zero + one)