package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex5 {

  def headOpt[A](l: List[A]): Option[A] = l match {
    case Cons(h, _) => Some(h)
    case Nil => None
  }

  def head[A](l: List[A]): A = headOpt(l).getOrElse(throw new NoSuchElementException("head of empty list"))

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    if (f(head(l))) dropWhile(l.tail)(f)
    else l


  trait Implicits {

    implicit class Ex5ListOpts[A](l: List[A]) {

      def headOpt: Option[A] = Ex5.headOpt(l)

      def head: A = Ex5.head(l)

      def dropWhile(f: A => Boolean): List[A] = Ex5.dropWhile(l)(f)

    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 1, 2, 3, 4, 5).dropWhile(_ < 3))
  }

}
