package pl.softech.learning.ch2

import pl.softech.learning.ch2.Ex4._

object Ex5 {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def main(args: Array[String]): Unit = {

    val add = uncurry(curry(Ex3.add))

    println(for (i <- 1 to 10) yield (i, add(i, 2)))

  }

}
