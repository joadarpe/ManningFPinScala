package chapter3

// A trait is an abstract interface that may optionally contain implementations of some methods
// Adding sealed in front means that all implementations of the trait must be declared in this file
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, t) => h * product(t)
  }

  // The function apply in the object List is a variadic function, meaning it accepts zero or more arguments of type A
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 3.1
    * What will be the result of the following match expression?
    */
  val x: Int = List(1, 2, 3, 4, 5) match {
    case Cons(h, Cons(2, Cons(4, _))) => h
    case Nil => 42
    case Cons(a, Cons(b, Cons(3, Cons(4, _)))) => a + b
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /**
    * Exercise 3.2
    * Implement the function tail for removing the first element of a List.
    * Note that the function takes constant time. What are different choices
    * you could make in your implementation if the List is Nil?
    * We’ll return to this question in the next chapter.
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  /**
    * Exercise 3.3
    * Using the same idea, implement the function setHead for replacing the first element of a List with a different value
    */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  /**
    * Exercise 3.4
    * Generalize tail to the function drop, which removes the first n elements from a list.
    * Note that this function takes time proportional only to the number of elements being dropped—
    * we don’t need to make a copy of the entire List.
    */
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (n > 0) drop(t, n - 1) else Cons(h, t)
  }

  /**
    * Exercise 3.5
    * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, t)
  }

  /**
    * DropWhile optimized and improved to assist with type inference.
    * Note the "if" inside de case and the definition now is curried to remove the need of typing the function f
    */
  def dropWhileNew[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhileNew(t)(f)
    case _ => as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /**
    * Exercise 3.6
    * Not everything works out so nicely. Implement a function, init, that returns a List
    * consisting of all but the last element of a List. So, given List(1,2,3,4), init will return List(1,2,3).
    * Why can’t this function be implemented in constant time like tail?
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sumFR(l: List[Int]): Int = {
    foldRight(l, 0)(_ + _)
  }

  def productFR(l: List[Double]): Double = {
    foldRight(l, 1.0)(_ * _)
  }

  /**
    * Exercise 3.7
    * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
    * Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
    * This is a deeper question that we’ll return to in chapter 5
    */
  // Not possible with actual implementation of foldRight

  /**
    * Exercise 3.8
    * See what happens when you pass Nil and Cons themselves to foldRight, like this:
    * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    * What do you think this says about the relationship between foldRight and the data constructors of List?
    */
  // The data constructor is just another operation

  /**
    * Exercise 3.9
    * Compute the length of a list using foldRight.
    */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  /**
    * Exercise 3.10
    * Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for large lists (we say it’s not stack-safe).
    * Convince yourself that this is the case, and then write another general list-recursion function, foldLeft, that is tail-recursive,
    * using the techniques we discussed in the previous chapter.
    */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
    * Exercise 3.11
    * Write sum, product, and a function to compute the length of a list using foldLeft.
    */
  def sumFL(ls: List[Int]): Int = foldLeft(ls, 0)(_ + _)

  def productFL(ls: List[Double]): Double = foldLeft(ls, 1.0)(_ * _)

  def lengthFL[A](ls: List[A]): Int = foldLeft(ls, 0)((x, _) => x + 1)

  /**
    * Exercise 3.12
    * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
    * See if you can write it using a fold.
    */
  def reverse[A](ls: List[A]): List[A] = {
    foldLeft(ls, Nil: List[A])((h, t) => Cons(t, h))
  }

  /**
    * Exercise 3.13
    * Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
    * Implementing foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively,
    * which means it works even for large lists without overflowing the stack.
    */
  def foldLeftWithFoldRight[A, B](ls: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(ls), z)((a, b) => f(b, a))
  }

  def foldRightWithFoldLeft[A, B](ls: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(ls), z)((b, a) => f(a, b))
  }

  /**
    * Exercise 3.14
    * Implement append in terms of either foldLeft or foldRight.
    */
  def appendFL[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  /**
    * Exercise 3.15
    * Hard: Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.
    */
  def concat[A](lss: List[List[A]]): List[A] = {
    foldRight(lss, Nil: List[A])(append)
  }

  /**
    * Exercise 3.16
    * Write a function that transforms a list of integers by adding 1 to each element.
    * (Reminder: this should be a pure function that returns a new List!)
    */
  def add1(ls: List[Int]): List[Int] = {
    foldRight(ls, Nil: List[Int])((h, t) => Cons(h + 1, t))
  }

  /**
    * Exercise 3.17
    * Write a function that turns each value in a List[Double] into a String.
    * You can use the expression d.toString to convert some d: Double to a String
    */
  def doubleToString(ls: List[Double]): List[String] = {
    foldRight(ls, Nil: List[String])((h, t) => Cons(h.toString, t))
  }

  /**
    * Exercise 3.18
    * Write a function map that generalizes modifying each element in a list
    * while maintaining the structure of the list.
    */
  def map[A, B](ls: List[A])(f: A => B): List[B] = {
    foldRight(ls, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  /**
    * Exercise 3.19
    * Write a function filter that removes elements from a list unless they satisfy a given predicate.
    * Use it to remove all odd numbers from a List[Int]
    */
  def filter[A](ls: List[A])(f: A => Boolean): List[A] = {
    foldRight(ls, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  /**
    * Exercise 3.20
    * Write a function flatMap that works like map except that the function given will return a list instead of a single result,
    * and that list should be inserted into the final resulting list.
    */
  def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] = {
    concat(map(ls)(f))
  }

  /**
    * Exercise 3.21
    * Use flatMap to implement filter.
    */
  def filterFM[A](ls: List[A])(f: A => Boolean): List[A] = {
    flatMap(ls)(a => if (f(a)) List(a) else Nil)
  }

  /**
    * Exercise 3.22
    * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
    * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    */
  def sumItems(lsa: List[Int], lsb: List[Int]): List[Int] = (lsa, lsb) match {
    case (Nil, b) => b
    case (a, Nil) => a
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sumItems(t1, t2))
  }

  /**
    * Exercise 3.23
    * Generalize the function you just wrote so that it’s not specific to integers or addition.
    * Name your generalized function zipWith.
    */
  def zipWith[A, B, C](lsa: List[A], lsb: List[B])(f: (A, B) => C): List[C] = (lsa, lsb) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  /**
    * Exercise 3.24
    * Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence.
    * For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
    * You may have some difficulty finding a concise purely functional implementation that is also efficient.
    * That’s okay. Implement the function however comes most naturally.
    * We’ll return to this implementation in chapter 5 and hopefully improve on it.
    * Note: Any two values x and y can be compared for equality in Scala using the expression x == y.
    */
  def hasSubsequence[A](ls: List[A], sub: List[A]): Boolean = ls match {
    case Nil => sub == Nil
    case _ if startsWith(ls, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

}