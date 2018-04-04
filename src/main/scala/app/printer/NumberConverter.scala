package app.printer

import app._

object NumberConverter {
  import app.verification.AccountNumberVerification._

  def convert(acc: AccountNumber): List[AccountNumber] = {
    alternativeAccountNumbers(acc).filter(verify(_).isRight)
  }

  def alternativeAccountNumbers(acc: AccountNumber): List[AccountNumber] =
    acc match {
      case AccountNumber(n1, n2, n3, n4, n5, n6, n7, n8, n9) ⇒
        n1.alternatives.map(AccountNumber(_, n2, n3, n4, n5, n6, n7, n8, n9)) ++
          n2.alternatives.map(AccountNumber(n1, _, n3, n4, n5, n6, n7, n8, n9)) ++
          n3.alternatives.map(AccountNumber(n1, n2, _, n4, n5, n6, n7, n8, n9)) ++
          n4.alternatives.map(AccountNumber(n1, n2, n3, _, n5, n6, n7, n8, n9)) ++
          n5.alternatives.map(AccountNumber(n1, n2, n3, n4, _, n6, n7, n8, n9)) ++
          n6.alternatives.map(AccountNumber(n1, n2, n3, n4, n5, _, n7, n8, n9)) ++
          n7.alternatives.map(AccountNumber(n1, n2, n3, n4, n5, n6, _, n8, n9)) ++
          n8.alternatives.map(AccountNumber(n1, n2, n3, n4, n5, n6, n7, _, n9)) ++
          n9.alternatives.map(AccountNumber(n1, n2, n3, n4, n5, n6, n7, n8, _))
    }

}
