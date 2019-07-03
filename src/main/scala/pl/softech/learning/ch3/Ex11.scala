package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex11 {

  def sum(l: List[Int]): Int = l.foldLeft(0)(_ + _)

  def product(l: List[Double]): Double = l.foldLeft(1d)(_ * _)

  def main(args: Array[String]): Unit = {

    println(sum(List(1, 2, 3)))
    println(product(List(1, 2, 3)))

  }

}
