package pl.softech.learning.ch6

import pl.softech.learning.ch6.RNG.Implicits._

object Ex1 {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, r) = rng.nextInt
    if (x == Int.MinValue) nonNegativeInt(r)
    else (Math.abs(x), r)
  }

  trait Implicits {

    implicit class Ex1Opts(r: RNG) {
      def nonNegativeInt: (Int, RNG) = Ex1.nonNegativeInt(r)
    }

  }

  def main(args: Array[String]): Unit = {
    println(SimpleRNG(1).nonNegativeInt)
  }

}
