package chapter4

sealed trait Either[+E, +A] {

  /**
    * Exercise 4.6
    * Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value
    */
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, AA >: A](b: Either[EE, AA]): Either[EE, AA] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    flatMap(a => b map (b1 => f(a, b1)))
  }

  /**
    * Once that map and flatMap are implemented we can use for-comprehensions
    */
  def map2FC[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this // flatMap (a =>
      b1 <- b // b map (b1 =>
    } yield f(a, b1)
  }
}

case class Left[+E](e: E) extends Either[E, Nothing]

case class Right[+A](a: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  /**
    * Exercise 4.7
    * Implement sequence and traverse for Either. These should return the first error that’s encountered, if there is one
    */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (tt => hh :: tt))
  }

  def sequenceFC[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case h :: t => for {
      hh <- h // h flatMap (hh =>
      tt <- sequence(t) // sequence(t) map (tt =>
    } yield hh :: tt
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => (f(h) map2 traverse(t)(f)) (_ :: _)
  }

  def sequenceT[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(a => a)
  }

}