package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._
import pl.softech.learning.ch5.Stream.cons

object Ex3 {

  def takeWhile[A](s: Stream[A], p: A => Boolean): Stream[A] = s match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) cons(h(), takeWhile(t(), p)) else Empty
  }


  trait Implicits {

    implicit class Ex3Opts[A](s: Stream[A]) {
      def takeWhile(p: A => Boolean): Stream[A] = Ex3.takeWhile(s, p)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).takeWhile(_ < 3).toList)
    println(Stream(1, 2, 3, 4).takeWhile(_ > 3).toList)
  }

}
