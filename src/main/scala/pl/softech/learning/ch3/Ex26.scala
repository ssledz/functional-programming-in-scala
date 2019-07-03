package pl.softech.learning.ch3

import pl.softech.learning.ch3.Tree.Implicits._

object Ex26 {

  def maximum(t: Tree[Int]): Int = {

    def maximum(t: Tree[Int], max: Int): Int = t match {

      case Leaf(v) => if (max > v) max else v
      case Branch(left, right) => maximum(left, max) max maximum(right, max)
      case null => max

    }

    maximum(t, Int.MinValue)

  }

  trait Implicits {

    implicit class Ex26TreeOpts(t: Tree[Int]) {
      def maximum: Int = Ex26.maximum(t)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Leaf(3).maximum)
    println(Branch(Leaf(3), null).maximum)
    println(Branch(Leaf(3), Leaf(6)).maximum)
    println(Branch(Leaf(3), Branch(Leaf(6), null)).maximum)
  }

}
