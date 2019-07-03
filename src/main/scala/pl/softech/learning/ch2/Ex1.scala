package pl.softech.learning.ch2

import scala.annotation.tailrec

object Ex1 {

  def fibRef(n: Int): Int = {
    if (n <= 1) n
    else fibRef(n - 2) + fibRef(n - 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def fib(n: Int, a: Int, b: Int): Int =
      if (n == 0) a
      else if (n == 1) b
      else fib(n - 1, b, a + b)

    fib(n, 0, 1)

  }

  def print(xs: Range, f: Int => Int): Unit = println(for (n <- xs) yield f(n))

  def main(args: Array[String]): Unit = {
    val xs = 0 to 10
    print(xs, fibRef)
    print(xs, fib)
  }


}
