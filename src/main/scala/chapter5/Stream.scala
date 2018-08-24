package chapter5

import chapter5.Stream.{cons, empty, unfold}

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

  /**
    * Exercise 5.13
    * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll.
    * The zipAll function should continue the traversal as long as either stream has more elements
    * —it uses Option to indicate whether each stream has been exhausted.
    */
  def mapU[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  def takeU(n: Int): Stream[A] = {
    unfold(this, n) {
      case (Cons(h, t), ni) if ni > 0 => Some(h(), (if (ni > 1) t() else empty, ni - 1))
      case _ => None
    }
  }

  def takeWhileU(f: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWithU[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }
  }

  def zipAllU[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h, t), empty) => Some((Some(h()), None), (t(), empty))
      case (empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case _ => None
    }
  }

  /**
    * Exercise 5.14
    * Hard: Implement startsWith using functions you’ve written. It should check if one Stream is a prefix of another.
    * For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
    */
  def startsWith[AB >: A](s2: Stream[AB]): Boolean = {
    zipAllU(s2) takeWhileU (_._2.isDefined) forAll (a => a._1 == a._2)
  }

  /**
    * Exercise 5.15
    * Implement tails using unfold.
    * For a given Stream, tails returns the Stream of suffixes of the input sequence, starting with the original Stream.
    * For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
    */
  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Cons(h, t) => Some(Cons(h, t), t())
      case _ => None
    }
  }

  def hasSubSequence[AB >: A](s: Stream[AB]): Boolean = {
    tails exists (_ startsWith s)
  }

  /**
    * Exercise 5.16
    * Hard: Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the intermediate results.
    */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight(z, Stream(z))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
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

  /**
    * Exercise 5.8
    * Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
    */
  def constant[B](a: B): Stream[B] = {
    cons(a, constant(a))
  }

  /**
    * Exercise 5.9
    * Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.
    */
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  /**
    * Exercise 5.10
    * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
    */
  def fib(): Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  /**
    * Exercise 5.11
    * Write a more general stream-building function called unfold. It takes an initial state,
    * and a function for producing both the next state and the next value in the generated stream.
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  /**
    * Exercise 5.12
    * Write fibs, from, constant, and ones in terms of unfold.
    */
  def fibU(): Stream[Int] = {
    unfold((0, 1))((a: (Int, Int)) => Some(a._1, (a._2, a._1 + a._2)))
  }

  def fromU(n: Int): Stream[Int] = {
    unfold(n)((a: Int) => Some(a, a + 1))
  }

  def constantU[B](a: B): Stream[B] = {
    unfold(a)((b: B) => Some(b, b))
  }

  def onesU: Stream[Int] = unfold(1)(_ => Some(1, 1))

}
