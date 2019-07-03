package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

import scala.annotation.tailrec

object Ex23 {

  def zipWith[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] = {
    @tailrec
    def zipWith(xs: List[A], ys: List[A], acc: List[A]): List[A] = (xs, ys) match {
      case (Cons(x, tl), Cons(y, tr)) => zipWith(tl, tr, Cons(f(x, y), acc))
      case (Nil, _) => acc
      case (_, Nil) => acc
    }

    zipWith(xs, ys, Nil).revers
  }

  trait Implicits {

    implicit class Ex23ListOpts[A](l: List[A]) {
      def zipWith(ys: List[A], f: (A, A) => A): List[A] = Ex23.zipWith(l, ys)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3).zipWith(List(2, 2), _ + _))
  }

}
