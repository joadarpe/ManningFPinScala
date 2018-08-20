package chapter5

import chapter5.Stream.{cons, empty}

sealed trait Stream[+A] {

  /**
    * Exercise 5.1
    * Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in the REPL.
    * You can convert to the regular List type in the standard library.
    * You can place this and other functions that operate on a Stream inside the Stream trait.
    */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /**
    * Exercise 5.2
    * Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first n elements of a Stream.
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
    * Exercise 5.3
    * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate
    */
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case Cons(_, _) => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsFR(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  /**
    * Exercise 5.4
    * Implement forAll, which checks that all elements in the Stream match a given predicate.
    * Your implementation should terminate the traversal as soon as it encounters a non-matching value.
    */
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  /**
    * Exercise 5.5
    * Use foldRight to implement takeWhile.
    */
  def takeWhileFR(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else empty)
  }

  /**
    * Exercise 5.6
    * Hard: Implement headOption using foldRight.
    */
  def headOption: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  /**
    * Exercise 5.7
    * Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument.
    */
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)
  }

  def append[B >: A](tail: => Stream[B]): Stream[B] = {
    foldRight(tail)((h, t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h) append t)
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val h = hd
    lazy val t = tl
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }
}
