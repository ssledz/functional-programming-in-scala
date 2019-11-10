package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch12.TraverseInstances._

object Ex15 {

  def main(args: Array[String]): Unit = {

    val F = Traverse[List]

    val xs = List(1, 2, 3, 4, 5)

    F.foldLeft(xs)(0)(_ + _) === 15

    F.foldLeft(xs)(List.empty[Int])((ys, y) => y :: ys) === xs.reverse

    F.foldRight(xs)(List.empty[Int])(_ :: _) === xs

  }

}
