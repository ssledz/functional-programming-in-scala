package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Applicative
import pl.softech.learning.ch11.ApplicativeInstances._
import pl.softech.learning.ch11.ApplicativeSyntax._

import scala.util.Try

object Ex1 {

  def main(args: Array[String]): Unit = {

    val F = Applicative[Option]

    val G = Applicative[List]

    F.replicateM(2, Option(2)) === Some(List(2, 2))

    G.replicateM(2, List(1)) === List(List(1, 1))

    (Option(1), Option(2)).mapN(_ + _) === Some(3)

    F.map3(Option(1), None, Option(2))(_ + _ + _) === None

    def parse(x: String): Option[Int] = Try(x.toInt).toOption

    F.traverse(List("1", "2"))(parse) === Some(List(1, 2))

    F.traverse(List("a"))(parse) === None

    F.sequence(List(Option(1), Some(2))) === Some(List(1, 2))

    F.product(Option(1), Option(2)) === Some((1, 2))

    F.product(Option(1), None) === None

  }

}
