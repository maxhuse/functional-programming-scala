package functional.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _}

sealed trait Either[+E,+A] {
  // 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)
  }

  // 4.6
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }

  // 4.6
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(_) => b
  }

  // 4.6
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      v1 <- this
      v2 <- b
    } yield f(v1, v2)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  // 4.7
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case x :: xs => f(x).map2(traverse(xs)(f))(_ :: _)
    }

  // 4.7
  def sequence[E, A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}