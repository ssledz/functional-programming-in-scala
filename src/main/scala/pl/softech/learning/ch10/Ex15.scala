package pl.softech.learning.ch10

import pl.softech.learning.ch10.FoldableInstances._
import pl.softech.learning.ch3.{Branch, Leaf, Tree}

object Ex15 {

  def toList[A, F[_] : Foldable](fa: F[A]): List[A] = {
    Foldable[F].foldRight(fa)(List.empty[A]) { (a, acc) =>
      a :: acc
    }
  }

  def main(args: Array[String]): Unit = {

    val tree: Tree[Int] = Branch(
      Branch(
        Branch(
          Branch(Leaf(1), Leaf(2)),
          Leaf(3),
        ),
        Leaf(4)
      ),
      Branch(Leaf(5), Leaf(6))
    )

    println(toList(tree))

    val seq = IndexedSeq(1, 2, 3, 4, 5, 6)

    println(toList(seq))

  }

}
