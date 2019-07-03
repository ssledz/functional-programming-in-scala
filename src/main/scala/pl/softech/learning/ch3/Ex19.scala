package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex19 {

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(head, tail) => if (f(head)) Cons(head, filter(tail)(f)) else filter(tail)(f)
    case Nil => Nil
  }

  trait Implicits {

    implicit class Ex19ListOpts[A](l: List[A]) {
      def filter(f: A => Boolean): List[A] = Ex19.filter(l)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3, 4, 5, 6, 7).filter(_ % 2 == 0))
  }

}
