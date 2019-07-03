package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex10 {

  def foldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {

    def foldLeft(l: List[A], acc: B): B = l match {
      case Cons(h, t) => foldLeft(t, f(h, acc))
      case Nil => acc
    }

    foldLeft(l, z)
  }

  trait Implicits {

    implicit class Ex10ListOpts[A](l: List[A]) {
      def foldLeft[B](z: B)(f: (A, B) => B): B = Ex10.foldLeft(l, z)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3).foldLeft(0)(_ + _))
    println(List(1, 2, 3).foldLeft(1)(_ * _))
  }

}
