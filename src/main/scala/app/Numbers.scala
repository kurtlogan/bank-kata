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
