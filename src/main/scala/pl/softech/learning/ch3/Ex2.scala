package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex2 {

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(h, t) => t
    case Nil => Nil
  }

  trait Implicits {

    implicit class Ex2ListOpts[A](l: List[A]) {
      def tail: List[A] = Ex2.tail(l)
    }

  }

  def main(args: Array[String]): Unit = {

    println(List(1, 2, 3, 4).tail)

  }

}
