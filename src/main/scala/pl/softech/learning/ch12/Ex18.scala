package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch12.TraverseInstances._

object Ex18 {

  def main(args: Array[String]): Unit = {

    val F = Traverse[List]

    val xs = List(1, 2, 3)

    val ys = List("a", "b", "c", "d")

    F.zipL(xs, ys) === List((1, Some("a")), (2, Some("b")), (3, Some("c")))

    F.zipL(ys, xs) === List(("a", Some(1)), ("b", Some(2)), ("c", Some(3)), ("d", None))

    F.zipR(xs, ys) === List((Some(1), "a"), (Some(2), "b"), (Some(3), "c"), (None, "d"))

  }

}
