package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex14 {

  def append[A](l: List[A], a: A): List[A] = l.foldRight(List(a))(Cons(_, _))


  trait Implicits {

    implicit class Ex14ListOpts[A](l: List[A]) {
      def append(a: A): List[A] = Ex14.append(l, a)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3).append(5))
  }

}
