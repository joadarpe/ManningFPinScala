import org.scalatest.{FreeSpec, Matchers}

/**
  * @author JonathanA.
  */
class Test extends FreeSpec with Matchers {

  "Test 1" in {
    1 + 2 should be (3)
  }
}
