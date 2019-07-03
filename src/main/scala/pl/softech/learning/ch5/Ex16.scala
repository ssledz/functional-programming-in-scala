package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex16 {

  def scanRight[A, B](s: Stream[A])(z: => B)(f: (A, => B) => B): Stream[B] = s.foldRight((z, Stream.cons(z, Stream.empty[B]))) {
    case (x, (res, xs)) => {
      val newRes = f(x, res)
      (newRes, Stream.cons(newRes, xs))
    }
  }._2

  trait Implicits {

    implicit class Ex16Opts[A](s: Stream[A]) {
      def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = Ex16.scanRight(s)(z)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
  }

}
