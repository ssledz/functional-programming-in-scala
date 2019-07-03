package pl.softech.learning.ch6

import pl.softech.learning.ch6.RNG.Implicits._
import pl.softech.learning.ch6.RNG._

object Ex5 {

  def nonNegativeEven: Rand[Int] = map(Ex1.nonNegativeInt)(i => i - i % 2)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def double(rng: RNG): (Double, RNG) = map(Ex1.nonNegativeInt)(x => x.toDouble / Int.MaxValue)(rng)

  trait Implicits {

    implicit class Ex5Opts[A](r: Rand[A]) {
      def map[B](f: A => B): Rand[B] = Ex5.map(r)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(1)
    println(rng.nextInt)
    println(int.map(_ + 4)(rng))
    println(nonNegativeEven(SimpleRNG(2)))
    println(rng.double)
    println(double(rng))
  }

}
