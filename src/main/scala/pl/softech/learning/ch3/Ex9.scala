package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex9 {

  def length[A](l: List[A]): Int = {
    def len(l: List[A], acc: Int): Int = l match {
      case Cons(_, t) => len(t, acc + 1)
      case Nil => acc
    }

    len(l, 0)
  }

  trait Implicits {


    implicit class Ex9ListOpts[A](l: List[A]) {
      def length: Int = Ex9.length(l)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3, 4).length)
  }

}
