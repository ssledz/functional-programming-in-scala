package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex7 {

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  trait Implicits {

    implicit class Ex7ListOpts[A](l: List[A]) {
      def foldRight[B](z: B)(f: (A, B) => B): B = Ex7.foldRight(l, z)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3).foldRight(0)(_ + _))
    println(List(1, 2, 3).foldRight(1)(_ * _))
  }

}
