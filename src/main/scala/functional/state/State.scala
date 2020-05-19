package functional.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (temp, nextRng) = rng.nextInt
    val next: Int = if (temp < 0) -(temp + 1) else temp
    (next, nextRng)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (next, nextRng) = nonNegativeInt(rng)
    (next.toDouble / (Int.MaxValue.toDouble + 1), nextRng)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextInt, nextRng1) = rng.nextInt
    val (nextDouble, nextRng2) = double(nextRng1)
    ((nextInt, nextDouble), nextRng2)
  }

  // 6.3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  // 6.3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (c == 0) {
        (acc, r)
      } else {
        val (i, r1) = r.nextInt
        loop(c - 1, r1, i :: acc)
      }
    }
    loop(count, rng, List())
  }

  // 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a1, r1) = ra(rng)
    val (a2, r2) = rb(r1)
    (f(a1, a2), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit[List[A]](List()))((el, acc) => map2(el, acc)(_ :: _))

  // 6.7
  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (f1, r1) = f(rng)
    g(f1)(r1)
  }

  // 6.8
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })

  // 6.9
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  // 6.9
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

case class State[S, +A](run: S => (A, S)) {
  import State._

  // 6.10
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  // 6.10
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  // 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object State {
  type Rand[A] = State[RNG, A]

  // 6.10
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  // 6.10
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((el, acc) => el.map2(acc)(_ :: _))

  // state combinators
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}
