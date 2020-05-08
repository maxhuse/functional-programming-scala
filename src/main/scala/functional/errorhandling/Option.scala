package functional.errorhandling

import scala.{Either => _, Option => _, Some => _, _}

sealed trait Option[+A] {
  // 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  // 4.1
  def getOrElse[B>:A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }

  // 4.1
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  // 4.1
  def flatMap2[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  // 4.1
  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(v) => Some(v)
    case None => ob
  }

  // 4.1
  def orElse2[B>:A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  // 4.1
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => Some(v)
    case _ => None
  }

  // 4.1
  def filter2(f: A => Boolean): Option[A] = this.flatMap(v => if (f(v)) Some(v) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(v1), Some(v2)) => Some(f(v1, v2))
      case _ => None
    }

  // 4.3
  def map2ViaFlatMap[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(v1 => b.map(v2 => f(v1, v2)))

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x :: xs => x.flatMap(v => sequence(xs).map(s => v :: s))
  }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)      // traverse(xs)(f).flatMap(rest => f(x).map(ff => ff :: rest))
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)

}