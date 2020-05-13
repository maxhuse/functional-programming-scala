package functional.parallelism

import java.util.concurrent._
import scala.language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit[List[A]](List()))((el, acc) => map2(el, acc)(_ :: _))

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => if (run(es)(cond).get) t(es) else f(es)

  // 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val ch = run(es)(n).get()
      run(es)(choices(ch))
    }

  // 7.12
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  // 7.13
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(pa).get
      run(es)(choices(k))
    }

  // 7.13
  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(pa).get
      run(es)(choices(k))
    }

  // 7.13
  def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(p)(i => choices(i))

  // 7.14
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  // 7.14
  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(p => p)

  // 7.14
  def flatMapViaJoin[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(choices))
}
