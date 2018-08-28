package chapter6

import org.scalatest.{FreeSpec, Matchers}

class RNGTest extends FreeSpec with Matchers {

  "Exercise 6.1 nonNegative" in {
    RNG.nonNegativeInt(SimpleRNG(15L))._1 == RNG.nonNegativeInt(SimpleRNG(15L))._1 shouldBe true
    RNG.nonNegativeInt(SimpleRNG(50000L))._1 >= 0 shouldBe true
    RNG.nonNegativeInt(SimpleRNG(0L))._1 >= 0 shouldBe true
  }

  "Exercise 6.2 double" in {
    val n = RNG.double(SimpleRNG(15L))._1
    n >= 0D && n <= 1D shouldBe true
  }

  "Exercise 6.3 intDouble" in {
    val ((i, d), _) = RNG.intDouble(SimpleRNG(15L))
    i >= 0 && d >= 0D && d <= 1D shouldBe true
  }

  "Exercise 6.3 doubleInt" in {
    val ((d, i), _) = RNG.doubleInt(SimpleRNG(15L))
    i >= 0 && d >= 0D && d <= 1D shouldBe true
  }

  "Exercise 6.3 double3" in {
    val ((d1, d2, d3), _) = RNG.double3(SimpleRNG(15L))
    d1 >= 0D && d1 <= 1D shouldBe true
    d2 >= 0D && d2 <= 1D shouldBe true
    d3 >= 0D && d3 <= 1D shouldBe true
  }

  "Exercise 6.4 ints" in {
    val list = RNG.ints(5)(SimpleRNG(15L))._1
    list.size shouldBe 5
  }

}
