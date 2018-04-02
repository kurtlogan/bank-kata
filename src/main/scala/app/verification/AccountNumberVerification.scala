package app.verification

import app._

object AccountNumberVerification {
  implicit def numberToFloat(n: Number): Float =
    Number.asInt(n).map(_.toFloat).getOrElse(0.05f)

  def verify(acc: AccountNumber): Either[Error, AccountNumber] =
    AccountNumber.asList(acc).reduceLeft[Number] {
      case (_, n @ Unknown(_)) ⇒ n
      case (acc, _)            ⇒ acc
    } match {
      case Unknown(_)         ⇒ Left(IllegibleError(acc))
      case _ if checksum(acc) ⇒ Right(acc)
      case _                  ⇒ Left(ChecksumError(acc))
    }

  private def checksum(acc: AccountNumber): Boolean =
    checksum(
      AccountNumber.asList(acc).reverse.zipWithIndex.foldLeft(0f) {
        case (a, (v, i)) ⇒ a + v * (i + 1)
      }
    )

  private def checksum(f: Float): Boolean = f % 11 == 0
}
