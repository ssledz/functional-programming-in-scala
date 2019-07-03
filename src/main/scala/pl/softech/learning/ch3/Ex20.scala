package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex20 {

  def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] = l.foldRight[List[B]](Nil) {
    (el, acc) => f(el).foldRight(acc)(Cons(_, _))
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    def iter(l: List[A], acc: List[List[B]]): List[List[B]] = l match {
      case Cons(head, tail) => iter(tail, Cons(f(head), acc))
      case Nil => acc
    }

    iter(l, Nil).revers.flatten
  }

  trait Implicits {

    implicit class Ex20ListOpts[A](l: List[A]) {
      def flatMap[B](f: A => List[B]): List[B] = Ex20.flatMap(l)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3, 4, 5).flatMap(e => List(e, e + 111)))
  }

}
