package functional.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25
  def size[A](t: Tree[A]): Int = t match {
    case null => 0
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // 3.26
  def maximum[A : Ordering](t: Tree[A], zero: A): A = t match {
    case null => zero
    case Leaf(v) => v
    case Branch(l, r) => implicitly[Ordering[A]].max(maximum(l, zero), maximum(r, zero))
  }

  // 3.27
  def depth[A](tree: Tree[A]): Int = {
    def loop(t: Tree[A], d: Int): Int = t match {
      case null => 0
      case Leaf(_) => d + 1
      case Branch(l, r) => math.max(loop(l, d + 1), loop(r, d + 1))
    }

    loop(tree, -1)
  }

  // 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case null => null
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29 worked only with non-null trees
  def fold[A, B](t: Tree[A])(mapper: A => B)(combiner: (B, B) => B): B = t match {
    case Leaf(v) => mapper(v)
    case Branch(l, r) => combiner(fold(l)(mapper)(combiner), fold(r)(mapper)(combiner))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold[A : Ordering](t: Tree[A]): A = fold(t)(a => a)((a, b) => implicitly[Ordering[A]].max(a, b))

  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)(1 + math.max(_, _))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
