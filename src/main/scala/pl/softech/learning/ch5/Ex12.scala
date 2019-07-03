package pl.softech.learning.ch5

import pl.softech.learning.ch5.Ex11.unfold
import pl.softech.learning.ch5.Stream.Implicits._

object Ex12 {

  def fibs: Stream[Int] = unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def from(n: Int): Stream[Int] = unfold(n)(acc => Some((acc, acc + 1)))

  def constant[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def ones: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def main(args: Array[String]): Unit = {
    println(from(3).take(5).toList)
    println(constant(2).take(10).toList)
    println(ones.take(3).toList)
    println(fibs.take(10).toList)
  }

}
