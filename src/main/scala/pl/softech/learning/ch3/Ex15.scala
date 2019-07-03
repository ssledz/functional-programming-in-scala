package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex15 {

  def addAll[A](xs: List[A], ys: List[A]): List[A] = {
    def addHelp[A](xs: List[A], acc: List[A]): List[A] = xs match {
      case Cons(h, t) => addHelp(t, Cons(h, acc))
      case Nil => acc
    }

    addHelp(xs.revers, ys)
  }

  def flatten[A](l: List[List[A]]): List[A] = l.foldLeft[List[A]](Nil)((e, acc) => acc ++ e)

  trait Implicits {

    implicit class Ex15ListOpts2[A](l: List[A]) {
      def ++(other: List[A]): List[A] = Ex15.addAll[A](l, other)
    }

    implicit class Ex15ListOpts[A](l: List[List[A]]) {
      def flatten: List[A] = Ex15.flatten(l)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3).revers)
    println(List(1, 2, 3) ++ List(4, 5))
    println(List(List(1, 2, 3), List(4), List(5, 6, 7)).flatten)
  }

}
