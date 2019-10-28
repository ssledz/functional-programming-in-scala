package pl.softech.learning.ch10

import pl.softech.learning.ch10.FoldableInstances._
import pl.softech.learning.ch3.{Branch, Leaf, Tree}

object Ex15 {

  def toList[A, F[_] : Foldable](fa: F[A]): List[A] = {
    Foldable[F].foldLeft(fa)(List.empty[A]) { (acc, a) =>
      a :: acc
    }
  }

  def main(args: Array[String]): Unit = {

    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    println(toList(tree))

    val seq = IndexedSeq(1, 2, 3, 4, 5, 6)
    
    println(toList(seq))

  }

}
