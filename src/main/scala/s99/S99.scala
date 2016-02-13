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

  def duplicate[T](ls: List[T]): List[T] = 
    ls flatMap (x => List(x, x))

  def duplicateN[T](n: Int, ls: List[T]): List[T] =
    ls flatMap (x => List.fill(n)(x))

  def drop[T](n: Int, ls: List[T]): List[T] =
    ls.zipWithIndex.filter(x => (x._2 + 1) % n != 0).map(x => x._1)

  def split[T](n: Int, ls: List[T]): (List[T], List[T]) = (n, ls) match {
    case (n, _) if n < 0 => throw new Exception
    case (_, Nil) => (Nil, Nil)
    case (0, ls) => (Nil, ls)
    case (n, ls) => {
      val (pre, post) = split(n - 1, ls.tail)
      (ls.head :: pre, post)
    }
  }

  def slice[T](from: Int, to: Int, ls: List[T]): List[T] = (from, to, ls) match {
    case (0, 0, _) => Nil
    case (0, m, xs) => xs.head :: slice(0, m - 1, xs.tail)
    case (n, m, xs) => slice(n - 1, m - 1, xs.tail)
  }

  def rotate[T](n: Int, ls: List[T]): List[T] = {
    if (n < 0) rotate(n + ls.size, ls)
    else {
      val m = n % ls.size
      val (pre, post) = split(m, ls)
      post ::: pre
    }
  }

  def removeAt[T](n: Int, ls: List[T]): (List[T], T) = (n, ls) match {
    case (_, Nil) => throw new Exception
    case (0, (x::xs)) => (xs, x)
    case (m, (x::xs)) => {
      val (ys, y) = removeAt(m - 1, xs)
      (x :: ys, y)
    }
  }
}
