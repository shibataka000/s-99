package s99.arithmetic

class S99Int(val start: Int) {
  import S99Int._

  def isPrime(): Boolean = start > 1 && primes.takeWhile(_ < start).forall(start % _ != 0)

  def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1

  def totient(): Int = (1 to start).count(this.isCoprimeTo(_))
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  def primes(): Stream[Int] = {
    def from(start: Int, step: Int = 1): Stream[Int] = start #:: from(start + step, step)
    def helper(l: Stream[Int]): Stream[Int] = l.head #:: helper(l.filter(_ % l.head != 0))
    2 #:: helper(from(3, 2))
  }

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}
