package functional

import scala.annotation.tailrec

object Chapter2 {

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    val length = as.length

    @tailrec
    def loop(i: Int): Boolean = {
      if (i + 1 >= length) true
      else if (!ordered(as(i), as(i + 1))) false
      else loop(i + 1)
    }

    if (length > 1) loop(0) else true
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}
