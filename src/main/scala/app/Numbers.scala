package app

sealed trait Number
case class Zero()   extends Number
case class One()    extends Number
case class Two()    extends Number
case class Three()  extends Number
case class Four()   extends Number
case class Five()   extends Number
case class Six()    extends Number
case class Seven()  extends Number
case class Eight()  extends Number
case class Nine()   extends Number
case object Unknown extends Number

object Zero {
  val analogue: String =
    " _ " +
      "| |" +
      "|_|"

  def unapply(arg: String): Option[Number] = arg match {
    case `analogue` ⇒ Some(apply())
    case _          ⇒ None
  }
}

object One {
  val analogue: String =
    "   " +
      "  |" +
      "  |"

  def unapply(arg: String): Option[Number] = arg match {
    case `analogue` ⇒ Some(apply())
    case _          ⇒ None
  }
}

object Two {
  val analogue: String =
    " _ " +
      " _|" +
      "|_ "

  def unapply(arg: String): Option[Number] = arg match {
    case `analogue` ⇒ Some(apply())
    case _          ⇒ None
  }
}

object Three {
  val analogue: String =
    " _ " +
      " _|" +
      " _|"

  def unapply(arg: String): Option[Number] = arg match {
    case `analogue` ⇒ Some(apply())
    case _          ⇒ None
  }
}

object Four {
  val analogue: String =
    "   " +
      "|_|" +
      "  |"

  def unapply(arg: String): Option[Number] = arg match {
    case `analogue` ⇒ Some(apply())
    case _          ⇒ None
  }
}

object Five {
  val analogue: String =
    " _ " +
      "|_" +
      " _|"

  def unapply(arg: String): Option[Number] = arg match {
    case `analogue` ⇒ Some(apply())
    case _          ⇒ None
  }
}

object Six {
  val analogue: String =
    " _ " +
      "|_ " +
      "|_|"

  def unapply(arg: String): Option[Number] = arg match {
    case `analogue` ⇒ Some(apply())
    case _          ⇒ None
  }
}

object Seven {
  val analogue: String =
    " _ " +
      "  |" +
      "  |"

  def unapply(arg: String): Option[Number] = arg match {
    case `analogue` ⇒ Some(apply())
    case _          ⇒ None
  }
}

object Eight {
  val analogue: String =
    " _ " +
      "|_|" +
      "|_|"

  def unapply(arg: String): Option[Number] = arg match {
    case `analogue` ⇒ Some(apply())
    case _          ⇒ None
  }
}

object Nine {
  val analogue: String =
    " _ " +
      "|_|" +
      " _|"

  def unapply(arg: String): Option[Number] = arg match {
    case `analogue` ⇒ Some(apply())
    case _          ⇒ None
  }
}
