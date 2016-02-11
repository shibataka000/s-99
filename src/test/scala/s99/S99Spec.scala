package s99

import org.specs2.mutable.Specification

class S99Spec extends Specification {
  "last" should {
    "find the last element of a list" in {
      S99.last(List(1, 1, 2, 3, 5, 8)) must beEqualTo(8)
    }
  }

  "penultimate" should {
    "find the last but one element of a list" in {
      S99.penultimate(List(1, 1, 2, 3, 5, 8)) must beEqualTo(5)
    }
  }

  "nth" should {
    "find the Kth element of a list" in {
      S99.nth(2, List(1, 1, 2, 3, 5, 8)) must beEqualTo(2)
    }
  }

  "length" should {
    "find the number of elements of a list" in {
      S99.length(List(1, 1, 2, 3, 5, 8)) must beEqualTo(6)
    }
  }

  "reverse" should {
    "Reverse a list" in {
      S99.reverse(List(1, 1, 2, 3, 5, 8)) must beEqualTo(List(8, 5, 3, 2, 1, 1))
    }
  }

  "isPalindrome" should {
    "Find out whether a list is a palindrome" in {
      S99.isPalindrome(List(1, 2, 3, 2, 1)) must beEqualTo(true)
    }
  }
}
