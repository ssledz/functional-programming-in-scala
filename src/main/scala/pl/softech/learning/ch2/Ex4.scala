package pl.softech.learning.ch2

import pl.softech.learning.ch2.Ex3.add

object Ex4 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def main(args: Array[String]): Unit = {

    val inc = curry(add)(1)

    println(for (i <- 1 to 10) yield (i, inc(i)))

  }

}
