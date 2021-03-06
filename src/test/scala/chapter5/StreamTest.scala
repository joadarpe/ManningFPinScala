package chapter5

import org.scalatest.{FreeSpec, Matchers}

class StreamTest extends FreeSpec with Matchers {

  "Exercise 5.1 toList" in {
    Stream(1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)
    Stream().toList shouldBe empty
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

  "Exercise 5.5 takeWhileFR" in {
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
    Stream.from(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "Exercise 5.10 fib" in {
    Stream.fib().take(5).toList shouldBe List(0, 1, 1, 2, 3)
  }

  "Exercise 5.11 unfold" in {
    val f = (a: Int) => if (a < 5) Some((a, a + 1)) else None
    Stream.unfold(1)(f).take(2).toList shouldBe List(1, 2)
    Stream.unfold(1)(f).take(50).toList shouldBe List(1, 2, 3, 4)
  }

  "Exercise 5.12 fibU" in {
    Stream.fibU().take(5).toList shouldBe List(0, 1, 1, 2, 3)
  }

  "Exercise 5.12 fromU" in {
    Stream.fromU(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "Exercise 5.12 constantU" in {
    Stream.constantU("a").take(5).toList shouldBe List("a", "a", "a", "a", "a")
    Stream.onesU.take(5).toList shouldBe List(1, 1, 1, 1, 1)
  }

  "Exercise 5.13 mapU" in {
    Stream(1, 2, 3, 4, 5).mapU(_.toString).toList shouldBe List("1", "2", "3", "4", "5")
  }

  "Exercise 5.13 takeU" in {
    Stream(1, 2, 3, 4, 5).takeU(2).toList shouldBe Stream(1, 2).toList
  }

  "Exercise 5.13 takeWhileU" in {
    Stream(1, 2, 3, 4, 5).takeWhileU(_ < 4).toList shouldBe List(1, 2, 3)
  }

  "Exercise 5.13 zipWithU" in {
    Stream(1, 2, 3).zipWithU(Stream("a", "b", "c"))((a, b) => a + b).toList shouldBe List("1a", "2b", "3c")
    Stream(1, 2, 3).zipWithU(Stream("a"))((a, b) => a + b).toList shouldBe List("1a")
  }

  "Exercise 5.13 zipAll" in {
    Stream(1, 2).zipAllU(Stream("a", "b")).toList shouldBe List((Some(1), Some("a")), (Some(2), Some("b")))
    Stream(1, 2).zipAllU(Stream("a")).toList shouldBe List((Some(1), Some("a")), (Some(2), None))
  }

  "Exercise 5.14 startsWith" in {
    Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldBe true
    Stream(1, 2, 3).startsWith(Stream(1, 2, 3)) shouldBe true
    Stream(1, 2, 3).startsWith(Stream(1, 3)) shouldBe false
    Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)) shouldBe false
  }

  "Exercise 5.15 tails" in {
    Stream().tails.map(a => a.toList).toList shouldBe List()
    Stream(1).tails.map(a => a.toList).toList shouldBe List(List(1))
    Stream(1, 2, 3).tails.map(a => a.toList).toList shouldBe List(List(1, 2, 3), List(2, 3), List(3))
  }

  "Exercise 5.15 hasSubSequence" in {
    Stream().hasSubSequence(Stream()) shouldBe false
    Stream(1).hasSubSequence(Stream()) shouldBe true
    Stream(1, 2).hasSubSequence(Stream(1)) shouldBe true
    Stream(1, 2, 3).hasSubSequence(Stream(1, 2, 3)) shouldBe true
    Stream(1, 2, 3).hasSubSequence(Stream(2, 3)) shouldBe true
    Stream(1, 2, 3).hasSubSequence(Stream(4, 5)) shouldBe false
  }

  "Exercise 5.16 scanRight" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
    Stream(1, 2, 3).scanRight(4)(_ + _).toList shouldBe List(10, 9, 7, 4)
  }

}
