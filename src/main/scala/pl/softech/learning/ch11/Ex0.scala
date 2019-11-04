package pl.softech.learning.ch11

import pl.softech.learning.ch11.ApplicativeSyntax._
import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.FunctorSyntax._
import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch11.MonadSyntax._

object Ex0 {

  def main(args: Array[String]): Unit = {

    println(FunctorLaws.law1(List(1, 2, 3)))

    val xs = List(1, 2, 3)

    Functor[List].map(xs)(_ + 1) === List(2, 3, 4)

    xs.fmap(_ + 1) === List(2, 3, 4)

    List(1, 2, 3).map2(List(4))(_ + _) === List(5, 6, 7)

    (List(1), List(2), List(3)).mapN(_ + _ + _) === List(6)

    (List.empty[Int], List(2), List(3)).mapN(_ + _ + _) === List.empty

    (Option(1), Option(2)).mapN(_ + _) === Some(3)

    (Option(1) >>= ((a: Int) => Option(a + 1))) === Some(2)

  }

}
