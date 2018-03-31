package app.parser

sealed trait Tokens
case object NewLine extends Tokens
case object Stop    extends Tokens
