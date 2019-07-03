package pl.softech.learning.ch2

object Ex6 {

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]): Unit = {

    val inc = (a: Int) => a + 1
    val even = (a: Int) => a * 2

    val odd = compose(inc, even)

    println(for (i <- 1 to 10) yield (i, odd(i)))

  }

}
