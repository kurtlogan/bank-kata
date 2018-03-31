package app.verification

import app.{ NumbersGenerator, UnitSpec }

class AccountNumberVerificationSpec extends UnitSpec with NumbersGenerator {
  "verify" should {
    "return true" when {
      "account number is valid" in {
        forAll(validAccountNumber) { n ⇒
          AccountNumberVerification.verify(n) shouldBe true
        }
      }
    }

    "return false" when {
      "account number is invalid" in {
        forAll(invalidAccountNumber) { n ⇒
          AccountNumberVerification.verify(n) shouldBe false
        }
      }
    }
  }
}
