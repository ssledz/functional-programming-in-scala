package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

import scala.annotation.tailrec

object Ex4 {


  def drop[A](n: Int, l: List[A]): List[A] = {

    @tailrec
    def drop(n: Int, acc: List[A]): List[A] =
      if (n == 0) acc
      else drop(n - 1, acc.tail)

    drop(n, l)
  }

  trait Implicits {

    implicit class Ex4ListOpts[A](l: List[A]) {
      def drop(n: Int): List[A] = Ex4.drop(n, l)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3, 4, 5).drop(2))
  }


}
