package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex4 {

  def forAll[A](s: Stream[A], p: A => Boolean): Boolean = s.foldRight(true){
    (a, acc) => p(a) &&  acc
  }

  trait Implicits {

    implicit class Ex4Opts[A](s: Stream[A]) {
      def forAll(p: A => Boolean): Boolean = Ex4.forAll(s, p)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).forAll(_ > 3))
    println(Stream(1, 2, 3, 4).forAll(_ < 5))
  }

}
