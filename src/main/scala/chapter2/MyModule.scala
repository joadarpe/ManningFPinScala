package chapter2

import scala.annotation.tailrec

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
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  /**
    * Exercise 2.1
    * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
    * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum
    * of the previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should
    * use a local tail-recursive function.
    *
    * @param n
    * @return
    */
  def fib(n: Int): Int = {
    @tailrec
    def go(n_2: Int, n_1: Int, n0: Int): Int =
      if (n <= 1) n
      else if (n0 == n) n_1 + n_2
      else go(n_1, n_2 + n_1, n0 + 1)

    go(0, 1, 2)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst[A](as: Array[A], f: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (f(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  /**
    * Exercise 2.2
    * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function
    *
    * @param as
    * @param ordered
    * @tparam A
    * @return
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(index: Int): Boolean = {
      if (as.length <= index) true
      else if (ordered(as(index - 1), as(index))) loop(index + 1)
      else false
    }

    loop(1)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  /**
    * Exercise 2.3
    * Let’s look at another example, currying, which converts a function f of two arguments
    * into a function of one argument that partially applies f.
    * Here again there’s only one implementation that compiles. Write this implementation.
    *
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }


  /**
    * Exercise 2.4
    * Implement uncurry, which reverses the transformation of curry.
    * Note that since => associates to the right, A => (B => C) can be written as A => B => C.
    *
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
    * Exercise 2.5
    * Implement the higher-order function that composes two functions.
    *
    * @param f
    * @param g
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibonacci", 6, fib))

    // Polymorphic with anonymous functions
    println("The index is %d".format(findFirst(Array(1, 2, 3), (a: Int) => a == 2)))
    println("Is Sorted? %b".format(isSorted(Array(1, 2, 3, 3), (a: Int, b: Int) => a <= b)))
    println("Is Sorted? %b".format(isSorted(Array("a", "ab", "abc", "abcd"), (a: String, b: String) => a <= b)))
  }

}
