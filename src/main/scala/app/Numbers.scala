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

  implicit class NumberOps(val value: Number) extends AnyVal {
    def asString: String = toMaybeInt.map(_.toString).getOrElse("?")

    def toMaybeInt: Option[Int] = value match {
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

    def toDigitalString: String = value match {
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

    def alternatives: List[Number] = value match {
      case _: Zero        ⇒ List(Eight())
      case _: One         ⇒ List(Seven())
      case _: Two         ⇒ List()
      case _: Three       ⇒ List(Nine())
      case _: Four        ⇒ List()
      case _: Five        ⇒ List(Six(), Nine())
      case _: Six         ⇒ List(Five(), Eight())
      case _: Seven       ⇒ List(One())
      case _: Eight       ⇒ List(Zero(), Six(), Nine())
      case _: Nine        ⇒ List(Three(), Five(), Eight())
      case n @ Unknown(_) ⇒ n.possibleNumbers
    }
  }

  def genericUnapply(n: Number, input: String): Option[Number] =
    if (n.toDigitalString == input) Some(n)
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
  def unapply(input: String): Option[Number] =
    Number.genericUnapply(Zero(), input)
}

object One {
  def unapply(input: String): Option[Number] =
    Number.genericUnapply(One(), input)
}

object Two {
  def unapply(input: String): Option[Number] =
    Number.genericUnapply(Two(), input)
}

object Three {
  def unapply(input: String): Option[Number] =
    Number.genericUnapply(Three(), input)
}

object Four {
  def unapply(input: String): Option[Number] =
    Number.genericUnapply(Four(), input)
}

object Five {
  def unapply(input: String): Option[Number] =
    Number.genericUnapply(Five(), input)
}

object Six {
  def unapply(input: String): Option[Number] =
    Number.genericUnapply(Six(), input)
}

object Seven {
  def unapply(input: String): Option[Number] =
    Number.genericUnapply(Seven(), input)
}

object Eight {
  def unapply(input: String): Option[Number] =
    Number.genericUnapply(Eight(), input)
}

object Nine {
  def unapply(input: String): Option[Number] =
    Number.genericUnapply(Nine(), input)
}

object Unknown {
  implicit class UnknownOps(val unknown: Unknown) extends AnyVal {
    def possibleNumbers: List[Number] =
      for {
        n ← List(
          Zero(),
          One(),
          Two(),
          Three(),
          Four(),
          Five(),
          Six(),
          Seven(),
          Eight(),
          Nine()
        )
        if unknown.value
          .zip(n.toDigitalString)
          .map(c ⇒ c._1 == c._2)
          .count(b ⇒ !b) == 1
      } yield n
  }
}
