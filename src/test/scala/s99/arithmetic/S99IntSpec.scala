package s99.arithmetic

import org.specs2.mutable.Specification

import s99.arithmetic.S99Int._

class S99IntSpec extends Specification {
  "isPrime" should {
    "Determine whether a given integer number is prime" in {
      1.isPrime must beEqualTo(false)
      6.isPrime must beEqualTo(false)
      7.isPrime must beEqualTo(true)
      8.isPrime must beEqualTo(false)
    }
  }

  "gcd" should {
    "Determine the greatest common divisor of two positive integer numbers" in {
      gcd(36, 63) must beEqualTo(9)
    }
  }

  "isCoprimeTo" should {
    "Determine whether two positive integer numbers are coprime" in {
      35.isCoprimeTo(64) must beEqualTo(true)
      36.isCoprimeTo(63) must beEqualTo(false)
    }
  }
}