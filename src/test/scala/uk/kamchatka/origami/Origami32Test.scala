package uk.kamchatka.origami

import org.scalatest.FlatSpec
import uk.kamchatka.origami.Origami32._

class Origami32Test extends FlatSpec {
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
}
