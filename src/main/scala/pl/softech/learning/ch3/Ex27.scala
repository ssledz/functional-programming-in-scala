package pl.softech.learning.ch3

import pl.softech.learning.ch3.Tree.Implicits._

object Ex27 {

  def depth[A](t: Tree[A]): Int = {

    def depth[A](t: Tree[A], acc: Int): Int = t match {

      case Leaf(_) => acc + 1
      case Branch(l, r) => depth(l, acc + 1) max depth(r, acc + 1)
      case null => acc

    }

    depth(t, 0)

  }

  trait Implicits {

    implicit class Ex27TreeOpts[A](t: Tree[A]) {
      def depth: Int = Ex27.depth(t)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Leaf(1).depth)
    println(Branch(Leaf(1), null).depth)
    println(Branch(Branch(Leaf(1), null), null).depth)


  }
}