package app

import app.parser.AccountNumber
import org.scalacheck.Gen

trait NumbersGenerator {
  lazy val unknownGen: Gen[Unknown] = Gen.oneOf(
    Unknown("|  |  |  "),
    Unknown("  |  |  |"),
    Unknown("_ |_ | _|")
  )

  lazy val singleNumberGen: Gen[Number] = for {
    unknown ← unknownGen
    result ← Gen.oneOf(
      Zero(),
      One(),
      Two(),
      Three(),
      Four(),
      Five(),
      Six(),
      Seven(),
      Eight(),
      Nine(),
      unknown
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
}
