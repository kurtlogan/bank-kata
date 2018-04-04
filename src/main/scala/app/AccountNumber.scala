package app

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
)

object AccountNumber {

  implicit class AccountNumberOps(val acc: AccountNumber) extends AnyVal {
    def toList: List[Number] =
      List(
        acc.n1,
        acc.n2,
        acc.n3,
        acc.n4,
        acc.n5,
        acc.n6,
        acc.n7,
        acc.n8,
        acc.n9
      )

    def foldLeft[A](empty: A)(f: (A, Number) ⇒ A): A =
      acc.toList.foldLeft(empty)(f)

    def foldRight[A](empty: A)(f: (Number, A) ⇒ A): A =
      acc.toList.foldRight(empty)(f)

    def reduceLeft[A >: Number](f: (A, Number) ⇒ A): A =
      acc.toList.reduceLeft[A](f)

    def reduceRight[A >: Number](f: (Number, A) ⇒ A): A =
      acc.toList.reduceRight[A](f)
  }

}
