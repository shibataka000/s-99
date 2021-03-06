package s99.arithmetic

import s99.workingWithList.WorkingWithList

class S99Int(val start: Int) {
  import S99Int._

  def isPrime(): Boolean = start > 1 && primes.takeWhile(_ < start).forall(start % _ != 0)

  def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1

  def totientP34(): Int = (1 to start).count(this.isCoprimeTo(_))

  def totientP37(): Int = start.primeFactorMultiplicity.map{
    case (p, m) => (p - 1) * math.pow(p, m - 1).toInt
  }.product

  def totient(): Int = totientP37

  def primeFactors(): List[Int] = {
    def helper(a: Int, ps: Stream[Int]): List[Int] = 
      if (a == 1) Nil
      else if (a % ps.head == 0) ps.head :: helper(a / ps.head, ps)
      else helper(a, ps.tail)
    if (start == 1) List(1)
    else helper(start, primes)
  }

  def primeFactorMultiplicity(): List[(Int, Int)] = 
    WorkingWithList.encode(start.primeFactors.sorted).map(_.swap)

  def goldbach(): (Int, Int) = primes.find(x => (start - x).isPrime) match {
    case None => throw new Exception
    case Some(x) => (x, start - x)
  }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  def primes(): Stream[Int] = {
    def from(start: Int, step: Int = 1): Stream[Int] = start #:: from(start + step, step)
    def helper(l: Stream[Int]): Stream[Int] = l.head #:: helper(l.filter(_ % l.head != 0))
    2 #:: helper(from(3, 2))
  }

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def listPrimesinRange(r: Range): List[Int] = primes.dropWhile(_ < r.head).takeWhile(_ <= r.last).toList
}
