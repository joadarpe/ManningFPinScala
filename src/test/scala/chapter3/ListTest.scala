package chapter3

import chapter3.List._
import org.scalatest.{FreeSpec, Matchers}

class ListTest extends FreeSpec with Matchers {

  "Exercise 3.1" in {
    x should be(3)
  }

  "Exercise 3.2" in {
    List.tail(Nil) should be(Nil)
    List.tail(List(1)) should be(Nil)
    List.tail(List(1, 2)) should be(List(2))
  }

  "Exercise 3.3" in {
    List.setHead(Nil, 1) should be(List(1))
    List.setHead(List(2, 3, 4), 1) should be(List(1, 3, 4))
  }

  "Exercise 3.4" in {
    println(s"The list after removing 1 elements is ${drop(List(1, 2, 3, 4, 5), 1)}")
    println(s"The list after removing 5 elements is ${drop(List(1, 2, 3), 5)}")
  }

  "Exercise 3.5" in {
    List.dropWhile(Nil, (a: Int) => a < 3) should be(Nil)
    List.dropWhile(List(1, 2, 3, 4, 5), (a: Int) => a < 3) should be(List(3, 4, 5))
    List.dropWhileNew(List(1, 2, 3, 4, 5))(a => a < 3) should be(List(3, 4, 5))
  }

  "Exercise 3.6" in {
    List.init(List(1, 2, 3, 4, 5)) should be(List(1, 2, 3, 4))
  }

  "Exercise 3.7" in {
  }

  "Exercise 3.8" in {
    List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) should be(List(1, 2, 3))
  }

  "Exercise 3.9" in {
    List.length(List(1, 2, 3, 4, 5, 6)) should be(6)
  }

  "Exercise 3.10" in {
    List.foldLeft(List(1, 2, 3), 0)(_ + _) should be(6)
  }

  "Exercise 3.11" in {
    List.sumFL(List(1, 2, 3)) should be(6)
    List.productFL(List(1, 2, 3)) should be(6.0)
    List.lengthFL(List(1, 2, 3)) should be(3)
  }

  "Exercise 3.12" in {
    List.reverse(Nil) should be(Nil)
    List.reverse(List(1, 2, 3)) should be(List(3, 2, 1))
  }

  "Exercise 3.13" in {}

  "Exercise 3.14" in {
    List.appendFL(Nil, Nil) should be(Nil)
    List.appendFL(Nil, List(1, 2, 3)) should be(List(1, 2, 3))
    List.appendFL(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
  }

  "Exercise 3.15" in {
    List.concat(List(List(1, 2), List(3, 4), List(5, 6))) should be(List(1, 2, 3, 4, 5, 6))
  }

  "Exercise 3.16" in {
    List.add1(List(0, 1, 2, 3)) should be(List(1, 2, 3, 4))
  }

  "Exercise 3.17" in {
    List.doubleToString(List(1.0, 2.0, 3.0)) should be(List("1.0", "2.0", "3.0"))
  }

  "Exercise 3.18" in {
    List.map(List(1.0, 2.0, 3.0))(a => a.toInt) should be(List(1, 2, 3))
  }

  "Exercise 3.19" in {
    List.filter(List(1, 2, 3, 4, 5, 6))(a => a % 2 == 0) should be(List(2, 4, 6))
  }

  "Exercise 3.20" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))
  }

  "Exercise 3.21" in {
    List.filterFM(List(1, 2, 3, 4, 5, 6))(a => a % 2 == 0) should be(List(2, 4, 6))
  }

  "Exercise 3.22" in {
    List.sumItems(List(1, 2, 3), Nil) should be(List(1, 2, 3))
    List.sumItems(Nil, List(1, 2, 3)) should be(List(1, 2, 3))
    List.sumItems(List(1, 2, 3), List(1, 2, 3)) should be(List(2, 4, 6))
    List.sumItems(List(1), List(1, 2, 3)) should be(List(2, 2, 3))
  }

  "Exercise 3.23" in {
    List.zipWith(List(1,2,3), Nil)((a, b) => s"$a $b") should be (Nil)
    List.zipWith(List(1,2,3), List("One", "Two", "Three"))((a, b) => s"$a $b") should be (List("1 One", "2 Two", "3 Three"))
  }

  "Exercise 3.24" in {
    List.hasSubsequence(List(1,2,3,4), List(1,2)) should be (true)
    List.hasSubsequence(List(1,2,3,4), List(2,3)) should be (true)
    List.hasSubsequence(List(1,2,3,4), List(4)) should be (true)
  }

}
