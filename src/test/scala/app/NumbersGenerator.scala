package app

import org.scalacheck.Gen

trait NumbersGenerator {
  val singleNumberGen = Gen.oneOf(
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

  val numberListGen = for {
    n    ← Gen.chooseNum(1, 500)
    list ← Gen.listOfN(n, singleNumberGen)
  } yield {
    list
  }
}
