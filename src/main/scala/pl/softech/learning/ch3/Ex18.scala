package pl.softech.learning.ch3

import List.Implicits._

object Ex18 {

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Cons(head, tail) => Cons(f(head), map(tail)(f))
    case Nil => Nil
  }

  trait Implicits {

    implicit class Ex17ListOpts[A](l: List[A]) {
      def map[B](f: A => B): List[B] = Ex18.map(l)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1,2,3,4).map(_ + 4))
  }

}
