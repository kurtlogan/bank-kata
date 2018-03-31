package app.verification

import app.{ NumbersGenerator, UnitSpec }

class AccountNumberVerificationSpec extends UnitSpec with NumbersGenerator {
  "verify" should {
    "succeed" when {
      "account number is valid" in {
        forAll(validAccountNumber) { n ⇒
          AccountNumberVerification.verify(n).merge shouldBe n
        }
      }
    }

    "fail" when {
      "account number is invalid" in {
        forAll(invalidAccountNumber) { n ⇒
          AccountNumberVerification.verify(n).merge shouldBe ChecksumError
        }
      }

      "account number is illegible" in {
        forAll(illegibleAccountNumbers) { n ⇒
          AccountNumberVerification.verify(n).merge shouldBe IllegibleError
        }
      }
    }
  }
}
