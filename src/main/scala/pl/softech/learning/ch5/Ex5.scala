package pl.softech.learning.ch5

import Stream.Implicits._

object Ex5 {

  def takeWhile[A](s: Stream[A], p: A => Boolean): Stream[A] = s.foldRight(Stream.empty[A]){
    (a, acc) => if(p(a)) Stream.cons(a, acc) else Stream.empty[A]
  }


  def main(args: Array[String]): Unit = {
    println(takeWhile[Int](Stream(1, 2, 3, 4), _ < 3).toList)
  }

}
