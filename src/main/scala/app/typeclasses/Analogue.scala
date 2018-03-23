package app.typeclasses

import app.Zero

trait Analogue[A] {
  def analogue(a: A): String
}

object Analogue {
  def apply[A](implicit an: Analogue[A]): Analogue[A] = an
  def analogue[A: Analogue](a: A): String             = Analogue[A].analogue(a)

  implicit class AnalogueOps[A](val a: A) extends AnyVal {
    def analogue(implicit an: Analogue[A]): String = an.analogue(a)
  }

  implicit val analogueZero: Analogue[Zero] = _ ⇒
    " _ " +
      "| |" +
    "|_|"
}
