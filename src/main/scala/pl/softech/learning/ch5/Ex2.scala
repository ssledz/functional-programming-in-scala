package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._
import pl.softech.learning.ch5.Stream.cons

object Ex2 {

  def take[A](s: Stream[A], n: Int): Stream[A] =
    if (n == 0) Empty
    else s match {
      case Empty => Empty
      case Cons(h, t) => cons(h(), take(t(), n - 1))
    }

  def drop[A](s: Stream[A], n: Int): Stream[A] = {
    def drop(s: Stream[A]): (Int, Stream[A]) = s match {
      case Empty => (0, Empty)
      case Cons(h, t) => drop(t()) match {
        case (c, s) => if (c == n) (c, cons(h(), s))
        else (c + 1, Empty)
      }
    }

    drop(s)._2

  }

  trait Implicits {

    implicit class Ex2Opts[A](s: Stream[A]) {
      def take(n: Int): Stream[A] = Ex2.take(s, n)

      def drop(n: Int): Stream[A] = Ex2.drop(s, n)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).take(3).toList)
    println(Stream(1, 2, 3, 4).take(3).drop(1).toList)
    println(Stream(1, 2, 3, 4).take(13).toList)
    println("drop")
    println(Stream(1, 2, 3, 4).drop(0).toList)
    println(Stream(1, 2, 3, 4).drop(1).toList)
    println(Stream(1, 2, 3, 4).drop(2).toList)
    println(Stream(1, 2, 3, 4).drop(3).toList)
    println(Stream(1, 2, 3, 4).drop(4).toList)
    println(Stream(1, 2, 3, 4).drop(43).toList)
  }

}
