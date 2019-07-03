package pl.softech.learning.ch5

import pl.softech.learning.ch5.Ex11.unfold
import pl.softech.learning.ch5.Stream.Implicits._

object Ex15 {

  def tails[A](s: Stream[A]): Stream[Stream[A]] = unfold(s) {
    case xs@Cons(_, t) => Some((xs, t()))
    case _ => None
  }

  trait Implicits {

    implicit class Ex15Opts[A](s: Stream[A]) {
      def tails: Stream[Stream[A]] = Ex15.tails(s)

      def hasSubsequence(s: Stream[A]): Boolean = tails exists (_ startsWith s)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).tails.map(_.toList).toList)
    println(Stream(1, 2, 3, 4).hasSubsequence(Stream(2, 3)))
    println(Stream(1, 2, 3, 4).hasSubsequence(Stream(2, 3, 1)))
  }

}
