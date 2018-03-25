package app.typeclasses

import app._

trait DigitalFormat[A] {
  def digital(a: A): String
}

object DigitalFormat {
  def apply[A](implicit an: DigitalFormat[A]): DigitalFormat[A] = an
  def digital[A: DigitalFormat](a: A): String                   = DigitalFormat[A].digital(a)

  implicit class AnalogueOps[A](val a: A) extends AnyVal {
    def digital(implicit an: DigitalFormat[A]): String = an.digital(a)
  }

  // format: off
  implicit val digitalZero: DigitalFormat[Zero] = _ ⇒
    " _ " +
    "| |" +
    "|_|"

  implicit val digitalOne: DigitalFormat[One] = _ ⇒
    "   " +
    "  |" +
    "  |"

  implicit val digitalTwo: DigitalFormat[Two] = _ ⇒
    " _ " +
    " _|" +
    "|_ "

  implicit val digitalThree: DigitalFormat[Three] = _ ⇒
    " _ " +
    " _|" +
    " _|"

  implicit val digitalFour: DigitalFormat[Four] = _ ⇒
    "   " +
    "|_|" +
    "  |"

  implicit val digitalFive: DigitalFormat[Five] = _ ⇒
    " _ " +
    "|_ " +
    " _|"

  implicit val digitalSix: DigitalFormat[Six] = _ ⇒
    " _ " +
    "|_ " +
    "|_|"

  implicit val digitalSeven: DigitalFormat[Seven] = _ ⇒
    " _ " +
    "  |" +
    "  |"

  implicit val digitalEight: DigitalFormat[Eight] = _ ⇒
    " _ " +
    "|_|" +
    "|_|"

  implicit val digitalNine: DigitalFormat[Nine] = _ ⇒
    " _ " +
    "|_|" +
    " _|"

  implicit val digitalUnknown: DigitalFormat[Unknown] = _.value


  // format: on
}
