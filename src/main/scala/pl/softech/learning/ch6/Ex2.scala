package pl.softech.learning.ch6

import pl.softech.learning.ch6.RNG.Implicits._

object Ex2 {

  def double(rng: RNG): (Double, RNG) = {
    val (x, r) = rng.nonNegativeInt
    (x.toDouble / Int.MaxValue, r)
  }

  trait Implicits {

    implicit class Ex2Opts(r: RNG) {
      def double: (Double, RNG) = Ex2.double(r)
    }

  }

  def main(args: Array[String]): Unit = {
    println(SimpleRNG(1).double)
  }

}
