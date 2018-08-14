package chapter3

// A trait is an abstract interface that may optionally contain implementations of some methods
// Adding sealed in front means that all implementations of the trait must be declared in this file
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // The function apply in the object List is a variadic function, meaning it accepts zero or more arguments of type A
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 3.1
    * What will be the result of the following match expression?
    */
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /**
    * Exercise 3.2
    * Implement the function tail for removing the first element of a List.
    * Note that the function takes constant time. What are different choices
    * you could make in your implementation if the List is Nil?
    * We’ll return to this question in the next chapter.
    *
    * @param l
    * @tparam A
    * @return
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  /**
    * Exercise 3.3
    * Using the same idea, implement the function setHead for replacing the first element of a List with a different value
    *
    * @param l
    * @param h
    * @tparam A
    * @return
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
    *
    * @param l
    * @param n
    * @tparam A
    * @return
    */
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (n > 0) drop(t, n - 1) else Cons(h, t)
  }

  /**
    * Exercise 3.5
    * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate
    *
    * @param l
    * @param f
    * @tparam A
    * @return
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, t)
  }

  /**
    * DropWhile optimized and improved to assist with type inference.
    * Note the "if" inside de case and the definition now is curried to remove the need of typing the function f
    *
    * @param as
    * @param f
    * @tparam A
    * @return
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
    *
    * @param l
    * @tparam A
    * @return
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
    *
    */
  // Not possible with actual implementation of foldRight

  /**
    * Exercise 3.8
    * See what happens when you pass Nil and Cons themselves to foldRight, like this:
    * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    * What do you think this says about the relationship between foldRight and the data constructors of List?
    *
    */
  // The data constructor is just another operation

  /**
    * Exercise 3.9
    * Compute the length of a list using foldRight.
    *
    * @param as
    * @tparam A
    * @return
    */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  /**
    * Exercise 3.10
    * Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for large lists (we say it’s not stack-safe).
    * Convince yourself that this is the case, and then write another general list-recursion function, foldLeft, that is tail-recursive,
    * using the techniques we discussed in the previous chapter.
    *
    * @param as
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
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
    *
    * @param args
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
    *
    * @param a1
    * @param a2
    * @tparam A
    * @return
    */
  def appendFL[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  /**
    * Exercise 3.15
    * Hard: Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.
    *
    * @param lss
    * @tparam A
    * @return
    */
  def flattens[A](lss: List[List[A]]): List[A] = {
    foldRight(lss, Nil: List[A])((h, t) => append(h, t))
  }
}