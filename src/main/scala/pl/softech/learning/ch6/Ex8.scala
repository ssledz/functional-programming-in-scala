package pl.softech.learning.ch6

import pl.softech.learning.ch6.Ex6.both
import pl.softech.learning.ch6.RNG.Implicits._
import pl.softech.learning.ch6.RNG._

object Ex8 {

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (x, r) = f(rng)
    g(x)(r)
  }

  def nonNegativeLessThan(n: Int)(rng: RNG): (Int, RNG) = flatMap(Ex1.nonNegativeInt) { x =>
    val mod = x % n
    if (x + (n - 1) - mod >= 0) r => (mod, r) else nonNegativeLessThan(n)
  }(rng)

  trait Implicits {

    implicit class Ex8Opts[A](r: Rand[A]) {
      def flatMap[B](g: A => Rand[B]): Rand[B] = Ex8.flatMap(r)(g)
    }

  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(1)
    println(both(int, int)(rng))
    val both2 = int.flatMap(x => int.map(y => (x, y)))
    println(both2(rng))
    val both3 = for {
      x <- int
      y <- int
    } yield (x, y)
    println(both3(rng))
    println(int.map2(int)(_ + _)(rng))
    val sum = for {
      x <- int
      y <- int
    } yield x + y

    println(sum(rng))

    println(nonNegativeLessThan(400000)(rng))
    println(nonNegativeLessThan(6)(new SimpleRNG(5)))
  }

}
