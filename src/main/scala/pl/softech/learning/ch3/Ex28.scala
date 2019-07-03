package pl.softech.learning.ch3

import pl.softech.learning.ch3.Tree.Implicits._

object Ex28 {

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {

    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case null => null

  }

  trait Implicits {

    implicit class Ex28TreeOpts[A](t: Tree[A]) {
      def map[B](f: A => B): Tree[B] = Ex28.map(t)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Leaf(2).map(_ + 2))
    println(Branch(Leaf(2), null).map(_ + 2))
    println(Branch(Leaf(2), Branch(Leaf(1), Leaf(2))).map(_ + 2))
  }

}
