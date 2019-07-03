package pl.softech.learning.ch2

object Ex3 {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)

  def add(a: Int, b: Int): Int = a + b

  def main(args: Array[String]): Unit = {

    val inc = partial1(1, add)

    println(for (i <- 1 to 10) yield (i, inc(i)))

  }

}
