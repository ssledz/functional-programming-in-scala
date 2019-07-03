package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex12 {

  def revers[A](l: List[A]): List[A] = l.foldLeft[List[A]](Nil)(Cons(_, _))

  trait Implicits {

    implicit class Ex12ListOpts[A](l: List[A]) {
      def revers: List[A] = Ex12.revers(l)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3, 4).revers)
  }

}
