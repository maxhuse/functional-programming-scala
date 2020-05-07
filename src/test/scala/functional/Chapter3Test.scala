package functional

import org.junit.Assert.assertEquals
import org.junit._

object Chapter3Test {

  class ListTest {
    import functional.List._

    val list: List[Int] = List(1, 2, 10, 0, -2)

    @Test def `List x`: Unit =
      assertEquals(
        3,
        x
      )

    @Test def `List tail`: Unit = {
      assertEquals(
        List(2, 10, 0, -2),
        tail(list)
      )

      assertEquals(
        Nil,
        tail(List(1))
      )
    }

    @Test def `List setHead`: Unit = {
      assertEquals(
        List(2, 2, 10, 0, -2),
        setHead(list, 2)
      )

      assertEquals(
        List(2),
        setHead(Nil, 2)
      )
    }

    @Test def `List drop`: Unit = {
      assertEquals(
        List(10, 0, -2),
        drop(list, 2)
      )

      assertEquals(
        Nil,
        drop(list, 10)
      )

      assertEquals(
        Nil,
        drop(Nil, 2)
      )
    }

    @Test def `List dropWhile`: Unit = {
      assertEquals(
        List(0, -2),
        dropWhile(list, (a: Int) => a > 0)
      )

      assertEquals(
        list,
        dropWhile(list, (a: Int) => a > 10)
      )

      assertEquals(
        Nil,
        dropWhile(Nil, (a: Int) => a > 0)
      )
    }

    @Test def `List init`: Unit = {
      assertEquals(
        List(1, 2, 10, 0),
        init(list)
      )

      assertEquals(
        Nil,
        init(List(1))
      )
    }

    @Test def `List length`: Unit = {
      assertEquals(
        5,
        length(list)
      )

      assertEquals(
        1,
        length(List(1))
      )

      assertEquals(
        0,
        length(Nil)
      )

      assertEquals(
        5,
        lengthViaFoldLeft(list)
      )
    }

    @Test def `List foldLeft`: Unit = {
      assertEquals(
        11,
        foldLeft(list, 0)(_ + _)
      )

      assertEquals(
        0,
        foldLeft(Nil: List[Int], 0)(_ + _)
      )
    }

    @Test def `List reverse`: Unit = {
      assertEquals(
        List(-2, 0, 10, 2, 1),
        reverse(list)
      )

      assertEquals(
        Nil,
        reverse(Nil)
      )
    }

    @Test def `List append2`: Unit = {
      assertEquals(
        List(1, 2, 3),
        appendViaFoldRight(List(1), List(2, 3))
      )

      assertEquals(
        List(4, 0),
        appendViaFoldRight(Nil, List(4, 0))
      )

      assertEquals(
        List(4, 0),
        appendViaFoldRight(List(4, 0), Nil)
      )
    }

    @Test def `List flatten`: Unit = {
      assertEquals(
        List(1, 2, 3, 0, 1),
        flatten(List(List(1), List(2, 3), List(0, 1)))
      )

      assertEquals(
        Nil,
        flatten(Nil)
      )

      assertEquals(
        List(1),
        flatten(List(List(1)))
      )
    }

    @Test def `List map`: Unit = {
      assertEquals(
        List(2, 3, 11, 1, -1),
        map(list)(_ + 1)
      )

      assertEquals(
        Nil,
        map(Nil: List[Int])(_ + 1)
      )
    }

    @Test def `List filter`: Unit = {
      assertEquals(
        List(10),
        filter(list)(_ >= 10)
      )

      assertEquals(
        Nil,
        filter(Nil: List[Int])(_ >= 10)
      )

      assertEquals(
        List(10),
        filterViaFlatMap(list)(_ >= 10)
      )

      assertEquals(
        Nil,
        filterViaFlatMap(Nil: List[Int])(_ >= 10)
      )
    }

    @Test def `List flatMap`: Unit = {
      assertEquals(
        List(1, 1, 2, 2, 10, 10, 0, 0, -2, -2),
        flatMap(list)(x => List(x, x))
      )

      assertEquals(
        Nil,
        flatMap(Nil: List[Int])(x => List(x, x))
      )
    }

    @Test def `List zipWith`: Unit = {
      assertEquals(
        List(5, 6, 6),
        zipWith(List(1, 3, 5), List(4, 3, 1))( _ + _)
      )

      assertEquals(
        Nil,
        zipWith(List(1, 3, 5), Nil: List[Int])( _ + _)
      )

      assertEquals(
        List(3, 8, 13),
        zipWith(List(1, 3, 5), List(2, 5, 8, 9))( _ + _)
      )
    }
  }

  class TreeTest {
    import functional.Tree._

    val treeInt: Tree[Int] = Branch(
      Branch(Leaf(2), null),
      Branch(
        Branch(Branch(Leaf(10), null), null), Branch(Leaf(3), Leaf(0))
      )
    )

    val treeString: Tree[String] = Branch(
      Branch(Leaf("abc"), null),
      Branch(
        Branch(Branch(Leaf("woof"), null), null), Branch(Leaf("aa"), Leaf("bark"))
      )
    )

    val treeNotNull: Tree[Int] = Branch(
      Branch(Leaf(2), Leaf(3)),
      Branch(
        Branch(Branch(Leaf(10), Leaf(9)), Leaf(4)), Branch(Leaf(3), Leaf(0))
      )
    )

    @Test def `Tree size`: Unit = {
      assertEquals(
        10,
        size(treeInt)
      )
    }

    @Test def `Tree maximum`: Unit = {
      assertEquals(
        10,
        maximum(treeInt, 0)
      )

      assertEquals(
        "woof",
        maximum(treeString, "")
      )
    }

    @Test def `Tree depth`: Unit = {
      assertEquals(
        4,
        depth(treeInt)
      )

      assertEquals(
        0,
        depth(Branch(null, null))
      )
    }

    @Test def `Tree map`: Unit = {
      assertEquals(
        Branch(
          Branch(Leaf(3), null),
          Branch(
            Branch(Branch(Leaf(11), null), null), Branch(Leaf(4), Leaf(1))
          )
        ),
        map(treeInt)(_ + 1)
      )
    }

    @Test def `Tree fold`: Unit = {
      assertEquals(
        Branch(
          Branch(Leaf(3), Leaf(4)),
          Branch(
            Branch(Branch(Leaf(11), Leaf(10)), Leaf(5)), Branch(Leaf(4), Leaf(1))
          )
        ),
        mapViaFold(treeNotNull)(_ + 1)
      )

      assertEquals(
        13,
        sizeViaFold(treeNotNull)
      )

      assertEquals(
        10,
        maximumViaFold(treeNotNull)
      )

      assertEquals(
        4,
        depthViaFold(treeNotNull)
      )

    }

  }
}
