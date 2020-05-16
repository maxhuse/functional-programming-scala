package functional.monoid

import functional.parallelism.Par
import functional.parallelism.Par._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero = Nil
  }

  // 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    val zero = 0
  }

  // 10.1
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    val zero = 1
  }

  // 10.1
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    val zero = false
  }

  // 10.1
  val booleanAnd: Monoid[Boolean] =  new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    val zero = true
  }

  // 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    val zero = None
  }

  // 10.3
  def endoMonoidCompose[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)

    val zero = a => a
  }

  // 10.3
  def endoMonoidAndThen[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1.andThen(a2)

    val zero = a => a
  }

  // 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))

  // 10.5
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoidAndThen[B])(a => b => f(b, a))(z)

  // 10.5
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoidCompose[B])(a => b => f(a, b))(z)

  // 10.7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.isEmpty)
      m.zero
    else if (as.length == 1)
      f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  // 10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero: Par[A] = Par.unit(m.zero)
    def op(a: Par[A], b: Par[A]): Par[A] = Par.map2(a, b)(m.op)
  }

  trait Foldable[F[_]] {
    def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(endoMonoidAndThen[B])(z)

    def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(a => (b: B) => f(a, b))(endoMonoidCompose[B])(z)

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((acc, a) => mb.op(acc, f(a)))

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
  }

  // 10.12
  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
      foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
  }

  // 10.14
  object OptionFoldable extends Foldable[Option] {
    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case None => mb.zero
        case Some(a) => f(a)
      }
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case None => z
      case Some(a) => f(z, a)
    }
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(a) => f(a, z)
    }
  }

  // 10.16
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)): (A, B) = (A.op(x._1, y._1), B.op(x._2, y._2))
      val zero: (A, B) = (A.zero, B.zero)
    }

  // 10.17
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
      def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))
      val zero: A => B = _ => B.zero
    }

}