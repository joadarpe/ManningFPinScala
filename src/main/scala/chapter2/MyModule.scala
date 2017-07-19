package chapter2

/**
  * @author JonathanA.
  */
// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  def fibonacci(n: Int): Int = {
    def go(n_2: Int, n_1: Int, n0: Int): Int =
      if (n <= 1) n
      else if (n0 == n) n_1 + n_2
      else go(n_1, n_2 + n_1, n0 + 1)
    go(0, 1, 2)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    formatResult("absolute value", -42, abs)
    formatResult("factorial", 7, factorial)
    formatResult("fibonacci", 6, fibonacci)
  }

}
