package app.verification

sealed trait Error
case object ChecksumError  extends Error
case object IllegibleError extends Error
