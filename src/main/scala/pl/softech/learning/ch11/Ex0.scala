package pl.softech.learning.ch11

import pl.softech.learning.ch11.ApplicativeInstances._
import pl.softech.learning.ch11.ApplicativeSyntax._
import pl.softech.learning.ch11.FunctorSyntax._

object Ex0 {

  def main(args: Array[String]): Unit = {

    val xs = List(1, 2, 3)

    println(Functor[List].map(xs)(_ + 1))

    println(xs.fmap(_ + 1))

    println(FunctorLaws.law1(List(1, 2, 3)))

    println(List(1, 2, 3).map2(List(4))(_ + _))

    println((List(1), List(2), List(3)).mapN(_ + _ + _))

    println((List.empty[Int], List(2), List(3)).mapN(_ + _ + _))

    println((Option(1), Option(2)).mapN(_ + _))

  }

}
