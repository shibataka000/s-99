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

  "flatten" should {
    "Flatten a nested list structure" in {
      S99.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) must beEqualTo(List(1, 1, 2, 3, 5, 8))
    }
  }

  "compress" should {
    "eliminate consecutive duplicates of list elements" in {
      S99.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must beEqualTo(List('a, 'b, 'c, 'a, 'd, 'e))
    }
  }

  "pack" should {
    "pack consecutive duplicates of list elements into sublists" in {
      S99.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must beEqualTo(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    }
  }

  "encode" should {
    "run-length encoding of a list" in {
      S99.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must beEqualTo(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
    }
  }

  "encodeModified" should {
    "modified run-length encoding" in {
      S99.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must beEqualTo(List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
    }
  }

  "decode" should {
    "decode a run-length encoded list" in {
      S99.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) must beEqualTo(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    }
  }

  "encodeDirect" should {
    "run-length encoding of a list (direct solution)" in {
      S99.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must beEqualTo(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
    }
  }

  "duplicate" should {
    "duplicate the elements of a list" in {
      S99.duplicate(List('a, 'b, 'c, 'c, 'd)) must beEqualTo(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    }
  }

  "duplicateN" should {
    "duplicate the elements of a list a given number of times" in {
      S99.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) must beEqualTo(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
    }
  }

  "drop" should {
    "drop every Nth element from a list" in {
      S99.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must beEqualTo(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    }
  }

  "split" should {
    "split a list into two parts" in {
      S99.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must beEqualTo((List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
      S99.split(2, List('a, 'b)) must beEqualTo((List('a, 'b),Nil))
      S99.split(-2, List('a, 'b)) must throwA[Exception]
    }
  }

  "slice" should {
    "extract a slice from a list" in {
      S99.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must beEqualTo(List('d, 'e, 'f, 'g))
    }
  }

  "rotate" should {
    "rotate a list N places to the left" in {
      S99.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must beEqualTo(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
      S99.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must beEqualTo(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    }
  }

  "removeAt" should {
    "remove the Kth element from a list" in {
      S99.removeAt(1, List('a, 'b, 'c, 'd)) must beEqualTo((List('a, 'c, 'd),'b))
    }
  }

  "insertAt" should {
    "insert an element at a given position into a list" in {
      S99.insertAt('new, 1, List('a, 'b, 'c, 'd)) must beEqualTo(List('a, 'new, 'b, 'c, 'd))
    }
  }

  "range" should {
    "create a list containing all integers within a given range" in {
      S99.range(4, 9) must beEqualTo(List(4, 5, 6, 7, 8, 9))
    }
  }

  "randomSelect" should {
    "extract a given number of randomly selected elements from a list" in {
      val selectedList = S99.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
      selectedList.size must beEqualTo(3)
      selectedList.forall(List('a, 'b, 'c, 'd, 'f, 'g, 'h).contains(_))
    }
  }

  "lotto" should {
    "draw N different random numbers from the set 1 .. M" in {
      val ls = S99.lotto(6, 49)
      ls.size must beEqualTo(6)
      ls.distinct.size must beEqualTo(6)
      ls.forall(x => x <= 49 && x >= 1)
    }
  }

  "randomPermute" should {
    "generate a random permutation of the elements of a list" in {
      val ls1 = List('a, 'b, 'c, 'd, 'e, 'f)
      val ls2 = S99.randomPermute(ls1)
      ls1.size must beEqualTo(ls2.size)
      (ls1 diff ls2).isEmpty must beEqualTo(true)
      (ls2 diff ls1).isEmpty must beEqualTo(true)
    }
  }

  "combination" should {
    "generate the combinations of K distinct objects chosen from the N elements of a list" in {
      S99.combination(2, List('a, 'b, 'c)) must beEqualTo(List(List('a, 'b), List('a, 'c), List('b, 'c)))
      S99.combination(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l)).size must beEqualTo(220)
    }
  }

  "group" should {
    "group the elements of a set into disjoint subsets" in {
      val expect = List(
        List(List('a, 'b), List('c)),
        List(List('a, 'c), List('b)),
        List(List('b, 'c), List('a))
      )
      S99.group(List(2, 1), List('a, 'b, 'c)) must beEqualTo(expect)
    }
  }
}
