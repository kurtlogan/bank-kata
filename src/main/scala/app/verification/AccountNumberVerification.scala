package app.verification

import app._

object AccountNumberVerification {
  private implicit def numberToFloat(n: Number): Float =
    n.toMaybeInt.map(_.toFloat).getOrElse(0.01f)

  def verify(acc: AccountNumber): Either[Error, AccountNumber] =
    acc.reduceLeft[Number] {
      case (_, n @ Unknown(_)) ⇒ n
      case (acc, _)            ⇒ acc
    } match {
      case Unknown(_)         ⇒ Left(IllegibleError(acc))
      case _ if checksum(acc) ⇒ Right(acc)
      case _                  ⇒ Left(ChecksumError(acc))
    }

  private def checksum(acc: AccountNumber): Boolean =
    checksum(
      acc.toList.reverse.zipWithIndex.foldLeft(0f) {
        case (a, (v, i)) ⇒ a + v * (i + 1)
      }
    )

  private def checksum(f: Float): Boolean = f % 11 == 0
}
