package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex14 {

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = s.zipAll(s2).foldRight(true) {
    case ((Some(x), Some(y)), acc) => (x == y) && acc
    case ((Some(_), None), _) => true
    case _ => false
  }

  trait Implicits {

    implicit class Ex14Opts[A](s: Stream[A]) {
      def startsWith[A](s2: Stream[A]): Boolean = Ex14.startsWith(s, s2)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4) startsWith Stream(1, 2))
    println(Stream(1, 2, 3, 4) startsWith Stream(3, 2))
    println(Stream(1, 2, 3, 4) startsWith Stream(2))
  }

}
