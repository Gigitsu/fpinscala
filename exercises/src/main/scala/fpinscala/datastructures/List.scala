package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = if (n < 0) l else l match {
    case Nil => Nil
    case Cons(_, tail) => drop(tail, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, tail) if f(h) => dropWhile(tail, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, tail) => Cons(x, init(tail))
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, tail) => foldLeft(tail, f(z, x))(f)
  }

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, List[A]())((acc, x) => Cons(x, acc))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((acc, x) => f(x, acc))
  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, List[A]())(append)

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(l, List[B]())((a, acc) => Cons(f(a), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRightViaFoldLeft(l, List[A]())((a, acc) => if(f(a)) Cons(a, acc) else acc)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if(f(a)) List(a) else Nil)

  def addPairwise(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (_, Nil) => Nil
    case (Cons(a, at), Cons(b, bt)) => Cons(a + b, addPairwise(at, bt))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, at), Cons(b, bt)) => Cons(f(a, b), zipWith(at, bt)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def go(p: List[A], b: List[A]): Boolean = (p, b) match {
      case (_, Nil) => true
      case (Cons(y, yt), Cons(z, zt)) if y == z => go(yt, zt)
      case _ => false
    }

    if (go(sup, sub)) true else hasSubsequence(tail(sup), sub)
  }
}
