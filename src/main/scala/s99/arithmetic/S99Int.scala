package s99.arithmetic

class S99Int(val start: Int) {
  import S99Int._

  def isPrime(): Boolean = start > 1 && (2 to (start - 1)).forall(start % _ != 0)

  def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}
