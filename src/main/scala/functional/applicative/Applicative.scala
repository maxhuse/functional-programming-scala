package functional.applicative

import functional.monad.Functor
import functional.monoid.Monoid
import functional.monoid.Monoid.Foldable
import functional.state.State

trait Applicative[F[_]] extends Functor[F] {
  // 12.2
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

  // 12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((a, b) => a(b))

  def unit[A](a: => A): F[A]

  // 12.2
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  // 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((el, acc) => map2(f(el), acc)(_ :: _))

  // 12.1
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  // 12.8
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  // 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty[K, V])) { case (acc, (mk, mv)) =>
      map2(acc, mv)((a, v) => a + ((mk, v)))
    }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] = flatMap(mf)(f => map(ma)(a => f(a)))
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Monad {
  // 12.5
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A,B](ma: Either[E, A])(f: A => Either[E, B]) = ma match {
      case Right(a) => f(a)
      case Left(b) => Left(b)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

}

object Applicative {

  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  // 12.6
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =  new Applicative[({type f[x] = Validation[E, x]})#f] {
    def unit[A](a: => A) = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E,B])(f: (A, B) => C) =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
        case (e@Failure(_, _), _) => e
        case (_, e@Failure(_, _)) => e
      }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({type f[x] = Const[M,  x]})#f] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] = traverse(fma)(ma => ma)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  // 12.14
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  import functional.applicative.Traverse.StateUtil._

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  def toList[A](fa: F[A]): List[A] = mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] = mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  // 12.17
  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = mapAccum(fa, z)((a, s) => ((), f(s, a)))._2

}

object Traverse {
  // 12.13
  val listTraverse = new Traverse[List] {
    override def traverse[M[_], A, B](l: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      l.foldRight(M.unit(List[B]()))((el, acc) => M.map2(f(el), acc)(_ :: _))
  }

  // 12.13
  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_], A, B](opt: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      opt match {
        case Some(a) => M.map(f(a))(Some(_))
        case None => M.unit(None)
      }
  }

  object StateUtil {
    def get[S]: State[S, S] =
      State(s => (s, s))

    def set[S](s: S): State[S, Unit] =
      State(_ => ((), s))
  }

}