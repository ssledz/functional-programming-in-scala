package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex8 {

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def main(args: Array[String]): Unit = {
    println(constant(2).take(10).toList)
    println(constant(4).take(10).toList)
  }

}
