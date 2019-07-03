package pl.softech.learning.ch6

import pl.softech.learning.ch6.RNG.Implicits._
import pl.softech.learning.ch6.RNG.Rand
import pl.softech.learning.ch6.RNG._

object Ex9 {

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = for {
    x <- ra
    y <- rb
  } yield f(x, y)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = s.flatMap(x => rng => (f(x), rng))

  def main(args: Array[String]): Unit = {
    val r = new SimpleRNG(1)
    println(map2(int, int)(_ + _)(r))
    println(int.map2(int)(_ + _)(r))
    println(int.map(_ + 1)(r))
    println(map(int)(_ + 1)(r))

  }

}
