package pl.softech.learning.ch5

import Stream.Implicits._

object Ex1 {

  def toList[A](s: Stream[A]): List[A] = s match {
    case Empty => Nil
    case Cons(h, t) => h() :: toList(t())
  }

  trait Implicits {

    implicit class Ex1Opts[A](s: Stream[A]) {
      def toList: List[A] = Ex1.toList(s)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4))
    println(Stream(1, 2, 3, 4).toList)
  }

}
