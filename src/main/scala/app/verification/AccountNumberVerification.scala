package app.verification

import app._

object AccountNumberVerification {
  implicit def numberToInt(n: Number): Float =
    Number.asFloat(n).map(_.toFloat).getOrElse(0.5f)

  def verify(acc: AccountNumber): Either[Error, AccountNumber] =
    acc match {
      case AccountNumber(Unknown(_), _, _, _, _, _, _, _, _) |
          AccountNumber(_, Unknown(_), _, _, _, _, _, _, _) |
          AccountNumber(_, _, Unknown(_), _, _, _, _, _, _) |
          AccountNumber(_, _, _, Unknown(_), _, _, _, _, _) |
          AccountNumber(_, _, _, _, Unknown(_), _, _, _, _) |
          AccountNumber(_, _, _, _, _, Unknown(_), _, _, _) |
          AccountNumber(_, _, _, _, _, _, Unknown(_), _, _) |
          AccountNumber(_, _, _, _, _, _, _, Unknown(_), _) |
          AccountNumber(_, _, _, _, _, _, _, _, Unknown(_)) ⇒
        Left(IllegibleError)
      case _ if checksum(acc) ⇒ Right(acc)
      case _                  ⇒ Left(ChecksumError)
    }

  private def checksum(acc: AccountNumber): Boolean =
    (acc.n9 + 2 * acc.n8 + 3 * acc.n7 + 4 * acc.n6 + 5 * acc.n5 + 6 * acc.n4 + 7 * acc.n3 + 8 * acc.n2 + 9 * acc.n1) % 11 == 0
}
