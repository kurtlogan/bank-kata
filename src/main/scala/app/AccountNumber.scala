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
  def asList(acc: AccountNumber): List[Number] =
    List(acc.n1, acc.n2, acc.n3, acc.n4, acc.n5, acc.n6, acc.n7, acc.n8, acc.n9)
}
