package app

sealed trait Number

case class Zero()                 extends Number
case class One()                  extends Number
case class Two()                  extends Number
case class Three()                extends Number
case class Four()                 extends Number
case class Five()                 extends Number
case class Six()                  extends Number
case class Seven()                extends Number
case class Eight()                extends Number
case class Nine()                 extends Number
case class Unknown(value: String) extends Number

object Number {

  def asInt(n: Number): Option[Int] = n match {
    case _: Zero    ⇒ Some(0)
    case _: One     ⇒ Some(1)
    case _: Two     ⇒ Some(2)
    case _: Three   ⇒ Some(3)
    case _: Four    ⇒ Some(4)
    case _: Five    ⇒ Some(5)
    case _: Six     ⇒ Some(6)
    case _: Seven   ⇒ Some(7)
    case _: Eight   ⇒ Some(8)
    case _: Nine    ⇒ Some(9)
    case _: Unknown ⇒ None
  }

  def asString(n: Number): String = n match {
    case _: Zero    ⇒ zero
    case _: One     ⇒ one
    case _: Two     ⇒ two
    case _: Three   ⇒ three
    case _: Four    ⇒ four
    case _: Five    ⇒ five
    case _: Six     ⇒ six
    case _: Seven   ⇒ seven
    case _: Eight   ⇒ eight
    case _: Nine    ⇒ nine
    case Unknown(s) ⇒ s
  }

  def matcher(n: Number, input: String): Option[Number] =
    if (asString(n) == input) Some(n)
    else None

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

object Zero {
  def unapply(input: String): Option[Number] = Number.matcher(Zero(), input)
}

object One {
  def unapply(input: String): Option[Number] = Number.matcher(One(), input)
}

object Two {
  def unapply(input: String): Option[Number] = Number.matcher(Two(), input)
}

object Three {
  def unapply(input: String): Option[Number] = Number.matcher(Three(), input)
}

object Four {
  def unapply(input: String): Option[Number] = Number.matcher(Four(), input)
}

object Five {
  def unapply(input: String): Option[Number] = Number.matcher(Five(), input)
}

object Six {
  def unapply(input: String): Option[Number] = Number.matcher(Six(), input)
}

object Seven {
  def unapply(input: String): Option[Number] = Number.matcher(Seven(), input)
}

object Eight {
  def unapply(input: String): Option[Number] = Number.matcher(Eight(), input)
}

object Nine {
  def unapply(input: String): Option[Number] = Number.matcher(Nine(), input)
}
