package pl.softech.learning.ch6

import pl.softech.learning.ch6.Ex5.double
import pl.softech.learning.ch6.RNG.Implicits._
import pl.softech.learning.ch6.RNG.{Rand, _}

object Ex6 {

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (x, r) = ra(rng)
      val (y, r2) = rb(r)
      (f(x, y), r2)
    }


  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  trait Implicits {

    implicit class Ex6Opts[A](r: Rand[A]) {
      def map2[B, C](rb: Rand[B])(f: (A, B) => C): Rand[C] = Ex6.map2(r, rb)(f)
    }

  }

  def main(args: Array[String]): Unit = {

    val rng = SimpleRNG(1)
    val (x, rng2) = int(rng)
    println((x, rng2))
    println(int(rng2))
    println(both(int, int)(rng))
    println(int.map2(int)(_ + _)(rng))
    println(randDoubleInt(rng))
    println(randIntDouble(rng))
  }

}
