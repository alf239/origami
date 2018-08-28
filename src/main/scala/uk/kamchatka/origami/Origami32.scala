package uk.kamchatka.origami

object Origami32 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[A](head: A, tail: List[A]) extends List[A]


  def wrap[A](a: A): List[A] = Cons(a, Nil)

  def nil(as: List[_]): Boolean = as match {
    case Nil => true
    case Cons(_, _) => false
  }

  def list[A](as: A*): List[A] =
    as.foldRight[List[A]](Nil)(Cons.apply)

  def foldL[A, B](f: (A, B) => B)(e: B, as: List[A]): B = as match {
    case Nil => e
    case Cons(x, xs) => f(x, foldL(f)(e, xs))
  }

  def mapL[A, B](f: A => B)(as: List[A]): List[B] =
    foldL[A, List[B]]((a, bs) => Cons(f(a), bs))(Nil, as)

  def appendL[A](a: List[A], b: List[A]): List[A] =
    foldL[A, List[A]](Cons(_, _))(b, a)

  def concatL[A](xs: List[List[A]]): List[A] =
    foldL[List[A], List[A]](appendL)(Nil, xs)

  def isort[A: Ordering](as: List[A]): List[A] = {
    def insert1(y: A, ys: List[A])(implicit ord: Ordering[A]): List[A] =
      foldL[A, (List[A], List[A])] { (x, pair) =>
        val (xs, yxs) = pair
        val Cons(y, _) = yxs
        val xs1 = Cons(x, xs)
        if (ord.lt(y, x)) (xs1, Cons(y, xs1))
        else (xs1, Cons(x, yxs))
      }((Nil, wrap(y)), ys)._2

    foldL(insert1)(Nil, as)
  }
}
