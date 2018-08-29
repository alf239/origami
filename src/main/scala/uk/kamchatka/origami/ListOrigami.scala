package uk.kamchatka.origami

object ListOrigami {

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

  def paraL[A, B](f: (A, (List[A], B)) => B)(e: B, as: List[A]): B = as match {
    case Nil => e
    case Cons(x, xs) => f(x, (xs, paraL(f)(e, xs)))
  }

  // or we can say p = f(_).isEmpty, g = f(_).get._1, b = u and use unfoldL
  def unfoldL1[A, B](f: B => Option[(A, B)])(u: B): List[A] =
    f(u).fold[List[A]](Nil) { case (x, v) => Cons(x, unfoldL1(f)(v)) }

  // or we can say f = b => if (p(b)) None else Some((f(b), g(b))), u = b and use unfoldL1
  def unfoldL[A, B](p: B => Boolean)(f: B => A, g: B => B, b: B): List[A] = {
    def basicUnfold[C](p: C => Boolean)(g: C => C, c: C): List[C] =
      if (p(c)) Nil else Cons(c, basicUnfold(p)(g, g(c))) // we can go further in a lazy language, but Scala isn't one

    mapL(f)(basicUnfold(p)(g, b))
  }

  def mapL[A, B](f: A => B)(as: List[A]): List[B] =
    foldL[A, List[B]]((a, bs) => Cons(f(a), bs))(Nil, as)

  def appendL[A](a: List[A], b: List[A]): List[A] =
    foldL[A, List[A]](Cons(_, _))(b, a)

  def concatL[A](xs: List[List[A]]): List[A] =
    foldL[List[A], List[A]](appendL)(Nil, xs)

  def isort[A: Ordering](as: List[A]): List[A] = {
    def insert1(y: A, ys: List[A])(implicit ord: Ordering[A]): List[A] =
      paraL[A, List[A]] { (x, pair) =>
        val (xs, yxs) = pair
        val Cons(y, _) = yxs
        if (ord.lt(y, x)) Cons(y, Cons(x, xs))
        else Cons(x, yxs)
      }(wrap(y), ys)

    foldL(insert1)(Nil, as)
  }
}