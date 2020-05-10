package functional.laziness

import java.lang.StackWalker

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  // 5.1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  // 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  // 5.2
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhileViaFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  // 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  // 5.7
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  // 5.7
  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  // 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  // 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  // 5.13
  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), i) if i > 1 => Some((h(), (t(), i - 1)))
    case (Cons(h, t), 1) => Some((h(), (empty, 0)))
    case _ => None
  }

  // 5.13
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  // 5.13
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  // 5.13
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
    case _ => None
  }

  // 5.14
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty).forAll { case(h1, h2) => h1 == h2  }

  // 5.15
  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case c => Some((c, c.drop(1)))
  }.append(empty)

  def hasSubsequence[B>:A](s: Stream[B]): Boolean = tails.exists(_.startsWith(s))

  // 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z))) {
    case (a, prev) => {
      // save laziness
      lazy val p = prev
      val next = f(a, p._1)
      (next, cons(next, p._2))
    }
  }._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val s: Stream[A] = Cons(() => a, () => s)
    s
  }

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.10
  def fibs(): Stream[Int] = {
    def loop(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, loop(n2, n1 + n2))
    }
    loop(0, 1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  // 5.12
  def fibsViaUnfold(): Stream[Int] = unfold((0, 1))(a => Some((a._1, (a._2, a._1 + a._2))))

  // 5.12
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(a => Some((a, a + 1)))

  // 5.12
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  // 5.12
  def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

}