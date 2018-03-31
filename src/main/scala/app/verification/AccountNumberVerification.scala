package app.verification

import app._

object AccountNumberVerification {
  implicit def numberToInt(acc: Number): Float = acc match {
    case _: Zero    ⇒ 0
    case _: One     ⇒ 1
    case _: Two     ⇒ 2
    case _: Three   ⇒ 3
    case _: Four    ⇒ 4
    case _: Five    ⇒ 5
    case _: Six     ⇒ 6
    case _: Seven   ⇒ 7
    case _: Eight   ⇒ 8
    case _: Nine    ⇒ 9
    case _: Unknown ⇒ .05f
  }

  def verify(acc: AccountNumber): Boolean =
    (acc.n9 + 2 * acc.n8 + 3 * acc.n7 + 4 * acc.n6 + 5 * acc.n5 + 6 * acc.n4 + 7 * acc.n3 + 8 * acc.n2 + 9 * acc.n1) % 11 == 0
}
