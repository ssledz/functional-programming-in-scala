package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex10 {

  def fibs: Stream[Int] = {
    def fibs(a: Int, b: Int): Stream[Int] = Stream.cons(a + b, fibs(b, a + b))

    Stream.cons(0, Stream.cons(1, fibs(0, 1)))
  }

  def main(args: Array[String]): Unit = {
    println(fibs.take(10).toList)
  }

}
