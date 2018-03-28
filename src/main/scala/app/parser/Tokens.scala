package app.parser

sealed trait Tokens
case class DigitalNumber(value: String) extends Tokens
case object Continuation                extends Tokens
case object Stop                        extends Tokens
