package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch12.TraverseInstances._

object Ex15 {

  def main(args: Array[String]): Unit = {

    val F = Traverse[List]

    F.foldLeft(List(1, 2, 3, 4, 5))(0)(_ + _) === 15

  }

}
