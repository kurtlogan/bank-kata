package app.parser

import app.Number

sealed trait Tokens
case class DigitalNumber(value: String) extends Tokens
case object NewLine                     extends Tokens
case object Stop                        extends Tokens
case class AccountNumber(
    n1: Number,
    n2: Number,
    n3: Number,
    n4: Number,
    n5: Number,
    n6: Number,
    n7: Number,
    n8: Number,
    n9: Number
) extends Tokens
