package chapter4

import org.scalatest.{FreeSpec, Matchers}

class OptionTest extends FreeSpec with Matchers {

  /**
    * Exercise 4.1
    * Implement all of the preceding functions on Option. As you implement each function,
    * try to think about what it means and in what situations you’d use it.
    * We’ll explore when to use each of these functions next.
    * Here are a few hints for solving this exercise:
    *
    * - It’s fine to use pattern matching, though you should be able to implement all the functions besides map
    * and getOrElse without resorting to pattern matching.
    * - For map and flatMap, the type signature should be enough to determine the implementation.
    * - getOrElse returns the result inside the Some case of the Option, or if the Option is None, returns the given default value.
    * - orElse returns the first Option if it’s defined; otherwise, it returns the second Option.
    */
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

}
