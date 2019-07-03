package pl.softech.learning.ch3

import pl.softech.learning.ch3.Tree.Implicits._

object Ex29 {

  def fold[A, B](t: Tree[A])(m: A => B)(f: (B, B) => B): B = t match {
    case Leaf(value) => m(value)
    case Branch(left, right) => f(fold(left)(m)(f), fold(right)(m)(f))
  }

  def size[A](t: Tree[A]): Int = t.fold(_ => 1)(_ + _)

  def depth[A](t: Tree[A]): Int = t.fold(_ => 1)(_ + 1 max _ + 1)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t.fold[Tree[B]](x => Leaf(f(x)))(Branch(_, _))

  def maximum(t: Tree[Int]): Int = t.fold(x => x)(_ max _)

  trait Implicits {

    implicit class Ex29TreeOpts[A](t: Tree[A]) {
      def fold[B](m: A => B)(f: (B, B) => B): B = Ex29.fold(t)(m)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(map(Branch(Leaf(2), Branch(Leaf(1), Leaf(2))))(_ + 2))
    println(maximum(Branch(Leaf(2), Branch(Leaf(33), Leaf(2)))))
    println(depth(Branch(Branch(Leaf(3), Branch(Leaf(3), Leaf(2))), Branch(Leaf(3), Branch(Leaf(3), Branch(Leaf(3), Branch(Leaf(3), Leaf(2))))))))
    println(depth(Branch(Leaf(1), Leaf(1))))
    println(size(Branch(Leaf(2), Branch(Leaf(3), Leaf(2)))))
  }

}
