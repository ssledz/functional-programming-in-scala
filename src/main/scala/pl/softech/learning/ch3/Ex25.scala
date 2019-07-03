package pl.softech.learning.ch3

import pl.softech.learning.ch3.Tree.Implicits._

object Ex25 {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(r) + size(l)
    case null => 0
  }

  trait Implicits {

    implicit class Ex25TreeOpts[A](t: Tree[A]) {
      def size: Int = Ex25.size(t)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Leaf(1).size)
    println(Branch(Leaf(1), null).size)
    println(Branch(Leaf(1), Leaf(2)).size)
  }

}
