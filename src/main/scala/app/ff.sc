import scala.annotation.tailrec
//
//sealed trait Number {
//  val digit: Int
//  val analogue: String
//}
//
//case object Zero extends Number {
//  override val digit: Int = 0
//  override val analogue =
//    " _ " +
//    "| |" +
//    "|_|"
//}
//
//case object One extends Number {
//  override val digit = 1
//  override val analogue =
//    "   " +
//    "  |" +
//    "  |"
//}
//
//val zero = Zero.analogue
//val one = One.analogue
//
//def parse(s: String): List[Number] = s.splitAt(9) match {
//    case (Zero.analogue, tail) => Zero :: parse(tail)
//    case (One.analogue, tail)  => One  :: parse(tail)
//    case (_, tail) => Unknown :: parse(tail)
//    case _ => Nil
//}
//
//parse(zero + one)


sealed trait Number[+A]
case class Zero() extends Number[Int]
case class One() extends Number[Int]
case class Two() extends Number[Int]
case class Three() extends Number[Int]
case class Four() extends Number[Int]
case class Five() extends Number[Int]
case class Six() extends Number[Int]
case class Seven() extends Number[Int]
case class Eight() extends Number[Int]
case class Nine() extends Number[Int]
case object Unknown extends Number[Nothing]

object Zero {
  val analogue: String =
    " _ " +
      "| |" +
      "|_|"

  def unapply(arg: String): Option[Number[Int]] = arg match {
    case `analogue` => Some(apply())
    case _ => None
  }
}

object a {
  val b = Zero.analogue match {
    case Zero(_) => "here"
    case _ => "none"
  }
}
a.b