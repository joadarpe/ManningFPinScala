package chapter4

import org.scalatest.{FreeSpec, Matchers}

class OptionTest extends FreeSpec with Matchers {

  "Exercise 4.1 map" in {
    None.map(a => a) shouldBe None
    Some(5).map(a => a.toString) shouldBe Some("5")
  }

  "Exercise 4.1 getOrElse" in {
    None.getOrElse(5) shouldBe 5
    Some(5).getOrElse(1) shouldBe 5
  }

  "Exercise 4.1 flatMap" in {
    None.flatMap(_ => None) shouldBe None
    Some(5).flatMap(a => Some(a.toString)) shouldBe Some("5")
  }

  "Exercise 4.1 orElse" in {
    None.orElse(None) shouldBe None
    None.orElse(Some(3)) shouldBe Some(3)
    Some(5).orElse(Some(2)) shouldBe Some(5)
  }

  "Exercise 4.1 filter" in {
    None.filter(_ => true) shouldBe None
    None.filter(_ => false) shouldBe None
    Some(5).filter(_ == 5) shouldBe Some(5)
    Some(5).filter(_ == 6) shouldBe None
  }

  "Exercise 4.2 variance" in {
    Option.mean(Seq(3.0, 3.0, 3.0)) shouldBe Some(3.0)
    Option.variance(Seq(3.0, 3.0, 3.0)) shouldBe Some(0.0)
  }

  "Exercise 4.3 map2" in {
    val sum = (a: Int, b: Int) => a + b
    Option.map2(None, None)(sum) shouldBe None
    Option.map2(None, Some(1))(sum) shouldBe None
    Option.map2(Some(1), None)(sum) shouldBe None
    Option.map2(Some(1), Some(2))(sum) shouldBe Some(3)
  }

  "Exercise 4.4 sequence" in {
    Option.sequence(List(Some(1), Some(2), None)) shouldBe None
    Option.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
  }

  "Exercise 4.5 traverse" in {
    Option.traverse(List(1, 2, 3))(a => Some(a.toString)) shouldBe Some(List("1", "2", "3"))
    Option.traversePM(List(1, 2, 3))(a => Some(a.toString)) shouldBe Some(List("1", "2", "3"))
    Option.sequenceT(List(Some(1), Some(2), None)) shouldBe None
    Option.sequenceT(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
  }

}
