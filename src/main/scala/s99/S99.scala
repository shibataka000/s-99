package s99

object S99 {
  def last[T](ls: List[T]): T = ls match {
    case x :: Nil => x
    case x :: xs => last(xs)
    case _ => throw new Exception
  }

  def penultimate[T](ls: List[T]): T = ls match {
    case x :: _ :: Nil => x
    case x :: xs => penultimate(xs)
    case _ => throw new Exception
  }

  def nth[T](n: Int, ls: List[T]): T = (n, ls) match {
    case (0, x :: _) => x
    case (n, _ :: xs) => nth(n - 1, xs)
    case _ => throw new Exception
  }

  def length[T](ls: List[T]): Int = ls match {
    case Nil => 0
    case x :: xs => 1 + length(xs)
  }

  def reverse[T](ls: List[T]): List[T] = ls match {
    case Nil => Nil
    case x :: xs => reverse(xs) ::: List(x)
  }

  def isPalindrome[T](ls: List[T]): Boolean = ls == ls.reverse

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case xs: List[_] => flatten(xs)
    case x => List(x)
  }

  def compress[T](ls: List[T]): List[T] = 
    ls.foldRight(List[T]())((a,b) => if (b.isEmpty || a != b.head) (a :: b) else b)

  def pack[T](ls: List[T]): List[List[T]] = 
    if (ls.isEmpty) Nil
    else {
      val (l1, l2) = ls span (_ == ls.head)
      l1 :: pack(l2)
    }

  def encode[T](ls: List[T]): List[(Int, T)] =
    pack(ls).map(xs => (xs.size, xs.head))

  def encodeModified[T](ls: List[T]): List[Any] =
    encode(ls).map {
      case (1, e) => e
      case (n, e) => (n ,e)
    }

  def decode[T](ls: List[(Int, T)]): List[T] = 
    ls.flatMap(x => List.fill(x._1)(x._2))

  def encodeDirect[T](ls: List[T]): List[(Int, T)] = 
    if (ls.isEmpty) Nil
    else {
      val (l1, l2) = ls span (_ == ls.head)
      (l1.size, l1.head) :: encodeDirect(l2)
    }

}
