package chapter6

trait RNG {

  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRG)
  }
}

object RNG {

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  /**
    * Exercise 6.1
    * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
    * Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    (if (n < 0) -(n + 1) else n, rng2)
  }

  /**
    * Exercise 6.2
    * Write a function to generate a Double between 0 and 1, not including 1.
    * Note: You can use Int.MaxValue to obtain the maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), rng2)
  }

  /**
    * Exercise 6.3
    * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
    * You should be able to reuse the functions you’ve already written.
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val (d, _) = double(rng)
    ((n, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), rng2) = intDouble(rng)
    ((d, n), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  /**
    * Exercise 6.4
    * Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = rng.nextInt match {
    case (n, rng1) if count > 1 => val (l, rng2) = ints(count - 1)(rng1); (n :: l, rng2)
    case (n, rng1) if count == 1 => (List(n), rng1)
  }


}
