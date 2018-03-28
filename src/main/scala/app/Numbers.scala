package app

sealed trait Number
case object Zero                  extends Number
case object One                   extends Number
case object Two                   extends Number
case object Three                 extends Number
case object Four                  extends Number
case object Five                  extends Number
case object Six                   extends Number
case object Seven                 extends Number
case object Eight                 extends Number
case object Nine                  extends Number
case class Unknown(value: String) extends Number

object Number {

  def asString(number: Number) = number match {
    case Zero       ⇒ zero
    case One        ⇒ one
    case Two        ⇒ two
    case Three      ⇒ three
    case Four       ⇒ four
    case Five       ⇒ five
    case Six        ⇒ six
    case Seven      ⇒ seven
    case Eight      ⇒ eight
    case Nine       ⇒ nine
    case Unknown(s) ⇒ s
  }

  // format: off

  val zero =
    " _ " +
    "| |" +
    "|_|"

  val one =
    "   " +
    "  |" +
    "  |"

  val two =
    " _ " +
    " _|" +
    "|_ "

  val three =
    " _ " +
    " _|" +
    " _|"

  val four =
    "   " +
    "|_|" +
    "  |"

  val five =
    " _ " +
    "|_ " +
    " _|"

  val six =
    " _ " +
    "|_ " +
    "|_|"

  val seven =
    " _ " +
    "  |" +
    "  |"

  val eight =
    " _ " +
    "|_|" +
    "|_|"

  val nine =
    " _ " +
    "|_|" +
    " _|"

  // format: on
}
