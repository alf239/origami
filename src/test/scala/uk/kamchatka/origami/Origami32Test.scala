package uk.kamchatka.origami

import org.scalacheck.Arbitrary
import org.scalatest.prop.Checkers
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.FlatSpec
import uk.kamchatka.origami.Origami32._

import scala.annotation.tailrec

class Origami32Test extends FlatSpec with Checkers {
  "foldL" should "calculate" in {
    assert(foldL[Double, Double](_ + _)(0.0, list(1.0, 2.0, 3.0)) == 6.0)
  }

  "mapL" should "map" in {
    assert(mapL[Double, String](_.toString)(list(1.0, 2.0, 3.0)) == list("1.0", "2.0", "3.0"))
    assert(mapL[Double, String](_.toString)(Nil) == Nil)
  }

  "appendL" should "append" in {
    assert(appendL(list(1, 2, 3), list(4, 5, 6)) == list(1, 2, 3, 4, 5, 6))
  }

  "concatL" should "flatten" in {
    assert(concatL(list(list(1, 2), list(3, 4), list(5), list(6), Nil)) ==
      list(1, 2, 3, 4, 5, 6))
  }

  "isort" should "sort" in {
    check((n: List[Int]) => sorted(isort(n)))
  }

  @tailrec
  final def sorted[A](ys: List[A])(implicit ord: Ordering[A]): Boolean = ys match {
    case Nil | Cons(_, Nil) => true
    case Cons(x, Cons(x1, xs: List[A])) => ord.lteq(x, x1) && sorted(Cons(x1, xs))
  }


  implicit def arbitraryList[A: Arbitrary]: Arbitrary[List[A]] = Arbitrary {
    Gen.oneOf(
      Gen.const(Nil),
      for {
        head <- arbitrary[A]
        tail <- arbitraryList.arbitrary
      } yield Cons(head, tail))
  }
}
