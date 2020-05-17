package functional.monad

import functional.parallelism.Par
import functional.parallelism.Par.Par
import functional.state.State

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor: Functor[List] = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // 11.3
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]()))((el, acc) => map2(el, acc)(_ :: _))

  // 11.3
  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]()))((el, acc) => map2(f(el), acc)(_ :: _))

  // 11.4
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    if (n <= 0) unit(List[A]())
    else map2(ma, replicateM(n - 1, ma))(_ :: _)

  // 11.7
  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(g)

  // 11.8
  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = compose((a: Unit) => ma, f)(())

  // 11.12
  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(a => a)

  // 11.13
  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(a => f(a)))
}

object Monad {

  // 11.1
  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  // 11.1
  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  // 11.1
  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    override def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  // 11.1
  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A) = List(a)
    override def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A,B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st.flatMap(f)
  }

  // 11.20
  def readerMonad[R] = new Monad[({type lambda[x] = Reader[R, x]})#lambda] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(st.run(r)).run(r))
  }
}

// 11.17
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)
