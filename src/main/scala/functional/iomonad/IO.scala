package functional.iomonad

import functional.monad.Monad
import scala.annotation.tailrec

object IO {
  sealed trait Free[F[_], A] {
    // 13.1
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
    // 13.1
    def map[B](f: A => B): Free[F, B] = flatMap(f.andThen(Return(_)))
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  // 13.1
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] =
    new Monad[({type f[a] = Free[F,a]})#f] {
      override def unit[A](a: => A): Free[F, A] = Return(a)
      override def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma.flatMap(f)
    }

  // 13.2
  @tailrec
  def runTrampoline[A](fr: Free[Function0, A]): A = fr match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(y, g) => runTrampoline(y.flatMap(yy => g(yy).flatMap(f)))
    }
  }

  @tailrec
  def step[F[_],A](fr: Free[F,A]): Free[F,A] = fr match {
    case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => fr
  }

  // 13.3
  def run[F[_], A](fr: Free[F, A])(implicit F: Monad[F]): F[A] = step(fr) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("Impossible, `step` eliminates these cases")
  }

}
