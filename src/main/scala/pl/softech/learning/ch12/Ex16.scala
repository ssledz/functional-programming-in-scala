package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch12.TraverseInstances._

object Ex16 {

  def main(args: Array[String]): Unit = {

    val F = Traverse[List]

    val H = Traverse[Tree]

    val tree = Tree(1, List(Tree(2, List.empty)))

    F.zipWithIndex2(List(1, 2, 3)) === List((1, 0), (2, 1), (3, 2))

    F.zipWithIndex(List(1, 2, 3)) === List((1, 0), (2, 1), (3, 2))

    F.toList2(List(1, 2)) === List(1, 2)

    F.toList(List(1, 2)) === List(1, 2)

    H.map(tree)(_ + 1) === Tree(2, List(Tree(3, List.empty)))

    H.foldRight(tree)(List.empty[Int])(_ :: _) === List(1, 2)

    H.foldLeft(tree)(List.empty[Int])((xs, x) => x :: xs) === List(2, 1)

    H.toList(tree) === List(1, 2)

    H.toList2(tree) === List(1, 2)

    H.toList2(H.zipWithIndex2(tree)) === List((1, 0), (2, 1))

    H.toList(H.zipWithIndex(tree)) === List((1, 0), (2, 1))

    H.zipWithIndex(tree) === Tree((1, 0), List(Tree((2, 1), List.empty)))

    F.reverse(List(1, 2, 3, 4)) === List(4, 3, 2, 1)

    H.reverse(tree) === Tree(2, List(Tree(1, List.empty)))

    val x = tree

    val y = List(3, 4, 5)

    H.toList(H.reverse(x)) ++ F.toList(F.reverse(y)) === F.reverse(F.toList(y) ++ H.toList(x))

  }

}
