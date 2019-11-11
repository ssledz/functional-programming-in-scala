package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.ApplicativeInstances._
import pl.softech.learning.ch12.TraverseInstances._

import scala.util.Try

object Ex19 {

  def main(args: Array[String]): Unit = {

    val F = Traverse[List]

    val G = F.compose[Option]

    val xs = List(Option("1"), Option("2"), Option("3"))

    def parse(x: String): Option[Int] = Try(x.toInt).toOption

    G.traverse(xs)(parse) === Some(List(Some(1), Some(2), Some(3)))

    val ys = List(Option(List("1", "2")), Option(List("3")))

    val H = G.compose[List]

    H.traverse(ys)(parse) === Some(List(Some(List(1, 2)), Some(List(3))))

  }

}
