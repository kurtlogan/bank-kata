package app

import org.scalacheck.Gen

trait NumbersGenerator {

  lazy val knownNumberGen: Gen[Number] = Gen.oneOf(
    Zero(),
    One(),
    Two(),
    Three(),
    Four(),
    Five(),
    Six(),
    Seven(),
    Eight(),
    Nine()
  )

  lazy val singleNumberGen: Gen[Number] = for {
    unknown ← unknownGen
    known   ← knownNumberGen
    result ← Gen.oneOf(
      known,
      unknown._1
    )
  } yield {
    result
  }

  lazy val accountNumberGen = for {
    pos1 ← singleNumberGen
    pos2 ← singleNumberGen
    pos3 ← singleNumberGen
    pos4 ← singleNumberGen
    pos5 ← singleNumberGen
    pos6 ← singleNumberGen
    pos7 ← singleNumberGen
    pos8 ← singleNumberGen
    pos9 ← singleNumberGen
  } yield {
    AccountNumber(pos1, pos2, pos3, pos4, pos5, pos6, pos7, pos8, pos9)
  }

  lazy val accountNumberListGen: Gen[List[AccountNumber]] =
    Gen.listOfN(500, accountNumberGen)

  lazy val validAccountNumber: Gen[AccountNumber] = Gen.oneOf(
    getAccountNumber("123456789"),
    getAccountNumber("000000051"),
    getAccountNumber("711111111"),
    getAccountNumber("777777177"),
    getAccountNumber("200800000"),
    getAccountNumber("333393333"),
    getAccountNumber("888886888"),
    getAccountNumber("888888880"),
    getAccountNumber("888888988"),
    getAccountNumber("555655555"),
    getAccountNumber("666566666"),
    getAccountNumber("686666666"),
    getAccountNumber("899999999"),
    getAccountNumber("993999999"),
    getAccountNumber("999959999"),
    getAccountNumber("490067115"),
    getAccountNumber("490067719"),
    getAccountNumber("490867715"),
    getAccountNumber("123456789"),
    getAccountNumber("000000051"),
    getAccountNumber("490867715"),
  )

  lazy val validAccountNumberListGen: Gen[List[AccountNumber]] =
    Gen.listOfN(500, validAccountNumber)

  lazy val invalidAccountNumber: Gen[AccountNumber] = Gen.oneOf(
    getAccountNumber("222222222"),
    getAccountNumber("444444444"),
    getAccountNumber("722777777")
  )

  lazy val illegibleAccountNumbers: Gen[AccountNumber] = Gen.oneOf(
    getAccountNumber("?????????"),
    getAccountNumber("111?222??"),
    getAccountNumber("333?456??"),
    getAccountNumber("12345678?"),
    getAccountNumber("?12345678")
  )

  lazy val ambiguousAccountNumbers: Gen[(AccountNumber, List[AccountNumber])] =
    Gen.oneOf(
      (
        getAccountNumber("888888888"),
        getAccountNumbers("888886888", "888888988", "888888880")
      ),
      (
        getAccountNumber("555555555"),
        getAccountNumbers("559555555", "555655555")
      ),
      (
        getAccountNumber("666666666"),
        getAccountNumbers("686666666", "666566666")
      ),
      (
        getAccountNumber("999999999"),
        getAccountNumbers("899999999", "993999999", "999959999")
      ),
      (
        getAccountNumber("490067715"),
        getAccountNumbers("490867715", "490067115", "490067719")
      )
    )

  // format: off

  lazy val unknownGen: Gen[(Unknown, Number)] = Gen.oneOf(
    (
      Unknown(
        "   " +
        "| |" +
        "|_|"
      ),
      Zero()
    ),
    (
      Unknown(
        "   " +
        "   " +
        "  |"
      ),
      One()
    ),
    (
      Unknown(
        " _ " +
        "  |" +
        "|_ "
      ),
      Two()
    ),
    (
      Unknown(
        "   " +
        " _|" +
        " _|"
      ),
      Three()
    ),
    (
      Unknown(
        "   " +
        "|_|" +
        "   "
      ),
      Four()
    ),
    (
      Unknown(
        " _ " +
        "|  " +
        " _|"
      ),
      Five()
    ),
    (
      Unknown(
        " _ " +
        " _ " +
        "|_|"
      ),
      Six()
    ),
    (
      Unknown(
        " _ " +
        "  |" +
        "   "
      ),
      Seven()
    ),
    (
      Unknown(
        "   " +
        "|_|" +
        "|_|"
      ),
      Eight()
    ),
    (
      Unknown(
        " _ " +
        "|_|" +
        " _ "
      ),
      Nine()
    )
  )

  // format: on

  private def getAccountNumbers(ss: String*): List[AccountNumber] = {
    ss.map(getAccountNumber).toList
  }

  private def getAccountNumber(s: String): AccountNumber = {
    val m = s.map {
      case '0' ⇒ Zero()
      case '1' ⇒ One()
      case '2' ⇒ Two()
      case '3' ⇒ Three()
      case '4' ⇒ Four()
      case '5' ⇒ Five()
      case '6' ⇒ Six()
      case '7' ⇒ Seven()
      case '8' ⇒ Eight()
      case '9' ⇒ Nine()
      case '?' ⇒ Unknown("?")
    }

    assert(m.length == 9)
    AccountNumber(m(0), m(1), m(2), m(3), m(4), m(5), m(6), m(7), m(8))
  }
}
