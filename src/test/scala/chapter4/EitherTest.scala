package chapter4

import org.scalatest.{FreeSpec, Matchers}

class EitherTest extends FreeSpec with Matchers {

  "mean" in {
    Either.mean(IndexedSeq()) shouldBe a[Left[_]]
    Either.mean(IndexedSeq(1.0, 2.0, 3.0)) shouldBe Right(2.0)
  }

  "safeDiv" in {
    Either.safeDiv(2, 0) shouldBe a[Left[_]]
    Either.safeDiv(2, 1) shouldBe Right(2)
  }

  "Try" in {
    Either.Try(5 / 0) shouldBe a[Left[_]]
    Either.Try(5) shouldBe Right(5)
  }

  "Exercise 4.6 map" in {
    val intToString = (a: Int) => a.toString
    Left().map(intToString) shouldBe a[Left[_]]
    Right(5).map(intToString) shouldBe Right("5")
  }

  "Exercise 4.6 flatMap" in {
    val intToString = (a: Int) => Right(a.toString)
    Left().flatMap(intToString) shouldBe a[Left[_]]
    Right(5).flatMap(intToString) shouldBe Right("5")
  }

  "Exercise 4.6 orElse" in {
    Left().orElse(Right(5)) shouldBe Right(5)
    Right(5).orElse(Right(6)) shouldBe Right(5)
  }

  "Exercise 4.6 map2" in {
    val sum = (a: Int, b: Int) => a + b
    Left().map2(Right(5))(sum) shouldBe Left()
    Right(5).map2(Right(6))(sum) shouldBe Right(11)
  }

  "Exercise 4.6 map2FC" in {
    val sum = (a: Int, b: Int) => a + b
    Left().map2(Right(5))(sum) shouldBe Left()
    Right(5).map2(Right(6))(sum) shouldBe Right(11)
  }

  "Exercise 4.7 sequence" in {
    Either.sequence(List(Right(1), Left())) shouldBe Left()
    Either.sequence(List(Right(1), Right(2))) shouldBe Right(List(1, 2))
  }

  "Exercise 4.7 sequenceFC" in {
    Either.sequenceFC(List(Right(1), Left())) shouldBe Left()
    Either.sequenceFC(List(Right(1), Right(2))) shouldBe Right(List(1, 2))
  }

  "Exercise 4.7 traverse" in {
    Either.traverse(List(1, 2))(Left(_)) shouldBe Left(1)
    Either.traverse(List(1, 2))(Right(_)) shouldBe Right(List(1, 2))
  }

  "Exercise 4.7 sequenceT" in {
    Either.sequenceT(List(Right(1), Left())) shouldBe Left()
    Either.sequenceT(List(Right(1), Right(2))) shouldBe Right(List(1, 2))
  }

}
