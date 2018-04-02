package app.verification

import app.AccountNumber

sealed trait Error
case class ChecksumError(acc: AccountNumber)  extends Error
case class IllegibleError(acc: AccountNumber) extends Error

object Error {}
