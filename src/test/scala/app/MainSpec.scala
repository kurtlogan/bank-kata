package app

class MainSpec extends UnitSpec with NumbersGenerator {
  "process" should {
    import Number._

    "process entire string" in {
      val end         = " " * 27 + "\n"
      val unknownOne  = unknowns("one")._1.toDigitalString
      val unknownZero = unknowns("zero")._1.toDigitalString

      val input =
        s"$zero$zero$zero\n$zero$zero$zero\n$zero$five$one\n$end" +
          s"$one$one$one\n$one$one$one\n$one$one$one\n$end" +
          s"$seven$seven$seven\n$seven$seven$seven\n$seven$seven$seven\n$end" +
          s"$two$zero$zero\n$zero$zero$zero\n$zero$zero$zero\n$end" +
          s"$three$three$three\n$three$three$three\n$three$three$three\n$end" +
          s"$eight$eight$eight\n$eight$eight$eight\n$eight$eight$eight\n$end" +
          s"$five$five$five\n$five$five$five\n$five$five$five\n$end" +
          s"$six$six$six\n$six$six$six\n$six$six$six\n$end" +
          s"$nine$nine$nine\n$nine$nine$nine\n$nine$nine$nine\n$end" +
          s"$four$nine$zero\n$zero$six$seven\n$seven$one$five\n$end" +
          s"$unknownOne$two$three\n$four$five$six\n$seven$eight$nine\n$end" +
          s"$zero$unknownZero$zero\n$zero$zero$zero\n$zero$five$one\n$end" +
          s"$four$nine$zero\n$eight$six$seven\n$seven$one$five\n$end" +
          s"$unknownZero$zero$zero\n$unknownOne$one$one\n$three$three$three\n$end"

      Main.process(input) shouldBe
        """000000051
          |711111111
          |777777177
          |200800000
          |333393333
          |888888888 AMB ['888886888', '888888988', '888888880']
          |555555555 AMB ['559555555', '555655555']
          |666666666 AMB ['686666666', '666566666']
          |999999999 AMB ['899999999', '993999999', '999959999']
          |490067715 AMB ['490867715', '490067115', '490067719']
          |123456789
          |000000051
          |490867715
          |?00?11333 ILL""".stripMargin
    }
  }
}
