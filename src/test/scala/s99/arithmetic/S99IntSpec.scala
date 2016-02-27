package s99.arithmetic

import org.specs2.mutable.Specification

import s99.arithmetic.S99Int._

class S99IntSpec extends Specification {
  "isPrime" should {
    "Determine whether a given integer number is prime" in {
      5.isPrime must beEqualTo(true)
      6.isPrime must beEqualTo(false)
      7.isPrime must beEqualTo(true)
      8.isPrime must beEqualTo(false)
    }
  }
}
