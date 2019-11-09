package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.ApplicativeInstances._
import pl.softech.learning.ch12.TraverseInstances._

import scala.util.Try

object Ex13 {

  def main(args: Array[String]): Unit = {

    val F = Traverse[List]

    def parse(x: String): Option[Int] = Try(x.toInt).toOption

    F.traverse(List("1", "2"))(parse) === Some(List(1, 2))

    F.traverse(List("1", "a"))(parse) === None

    val G = Traverse[Option]

    G.traverse(Option(1))(x => List(x - 1, x, x + 1)) === List(Some(0), Some(1), Some(2))

    G.traverse(None: Option[Int])(x => List(x - 1, x, x + 1)) === List(None)

    val H = Traverse[Tree]

    H.traverse(Tree("1", List.empty))(parse) === Some(Tree(1, List.empty))

    H.traverse(Tree("1", List(Tree("2", List.empty))))(parse) === Some(Tree(1, List(Tree(2, List.empty))))

    H.traverse(Tree("a", List.empty))(parse) === None

  }

}
