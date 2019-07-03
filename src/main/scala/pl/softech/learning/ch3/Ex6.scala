package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex6 {

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("init of empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  trait Implicits {

    implicit class Ex6ListOpts[A](l: List[A]) {
      def init: List[A] = Ex6.init(l)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3, 4, 5).init)
  }

}
