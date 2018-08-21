package chapter5

import org.scalatest.{FreeSpec, Matchers}

class StreamTest extends FreeSpec with Matchers {

  "Exercise 5.1 toList" in {
    Stream(1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "Exercise 5.2 take" in {
    Stream(1, 2, 3, 4, 5).take(2).toList shouldBe Stream(1, 2).toList
  }

  "Exercise 5.2 drop" in {
    Stream(1, 2, 3, 4, 5).drop(3).toList shouldBe Stream(4, 5).toList
  }

  "Exercise 5.3 takeWhile" in {
    Stream(1, 2, 3, 4, 5).takeWhile(a => a < 4).toList shouldBe List(1, 2, 3)
  }

  "Exercise 5.4 forAll" in {
    Stream(1, 2, 3, 4, 5, 6, 7, 8, 9).forAll(_ < 5) shouldBe false
  }

  "Exercise 5.5 takeWhile" in {
    Stream(1, 2, 3, 4, 5).takeWhileFR(_ < 4).toList shouldBe List(1, 2, 3)
  }

  "Exercise 5.6 headOption" in {
    Stream(1, 2, 3, 4, 5).headOption shouldBe Some(1)
  }

  "Exercise 5.7 map" in {
    Stream(1, 2, 3, 4, 5).map(_.toString).toList shouldBe List("1", "2", "3", "4", "5")
  }

  "Exercise 5.7 filter" in {
    Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList shouldBe List(2, 4)
  }

  "Exercise 5.7 append" in {
    Stream(1, 2, 3).append(Stream(4, 5)).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "Exercise 5.7 flatMap" in {
    Stream(1, 2).flatMap(a => Stream(a, a)).toList shouldBe List(1, 1, 2, 2)
  }

  "Exercise 5.8 constant" in {
    val ones: Stream[Int] = Stream.constant(1)
    ones.take(5).toList shouldBe List(1, 1, 1, 1, 1)
    ones.exists(_ % 2 >= 0) shouldBe true
    ones.map(_ + 1) exists (_ % 2 == 0) shouldBe true
  }

  "Exercise 5.9 from" in {
    val from: Stream[Int] = Stream.from(1)
    from.take(5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "Exercise 5.10" in {
    Stream.fib().take(5).toList shouldBe List(0, 1, 1, 2, 3)
  }

  "Exercise 5.11" in {
    val f = (a: Int) => if (a < 5) Some((a, a + 1)) else None
    Stream.unfold(1)(f).take(2).toList shouldBe List(1, 2)
    Stream.unfold(1)(f).take(50).toList shouldBe List(1, 2, 3, 4)
  }


}
