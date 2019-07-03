package pl.softech.learning.ch6

import pl.softech.learning.ch6.Ex2._
import pl.softech.learning.ch6.RNG.Implicits._

object Ex3 {

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (x, r) = rng.nextInt
    val (y, r2) = double(r)
    ((x, y), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((x, y), r) = intDouble(rng)
    ((y, x), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (x, r) = double(rng)
    val (y, r1) = double(r)
    val (z, r2) = double(r1)
    ((x, y, z), r2)
  }

  trait Implicits {

    implicit class Ex3Opts(r: RNG) {

      def intDouble: ((Int, Double), RNG) = Ex3.intDouble(r)

      def doubleInt: ((Double, Int), RNG) = Ex3.doubleInt(r)

      def double3: ((Double, Double, Double), RNG) = Ex3.double3(r)

    }

  }

  def main(args: Array[String]): Unit = {
    println(SimpleRNG(1).intDouble)
    println(SimpleRNG(2).doubleInt)
    println(SimpleRNG(3).double3)
  }

}
