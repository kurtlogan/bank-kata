case class AccountNumber(n1: Int, n2: Int, n3: Int)

def r(a: AccountNumber): List[AccountNumber] = a match {
  case AccountNumber(n1, n2, n3) =>
    loop.map(AccountNumber(_, n2, n3)) ++
    loop.map(AccountNumber(n1, _, n3)) ++
    loop.map(AccountNumber(n1, n2, _))
}

def loop = (for(i <- 0 to 9) yield i).toList