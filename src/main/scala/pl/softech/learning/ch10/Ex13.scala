package pl.softech.learning.ch10

import pl.softech.learning.ch10.FoldableInstances._
import pl.softech.learning.ch3.{Branch, Leaf, Tree}

object Ex13 {

  trait FoldableInstances {

    implicit val foldableTreeInstance: Foldable[Tree] = new Foldable[Tree] {

      def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
        case Leaf(a) => f(z, a)
        case Branch(left, Leaf(a)) => f(foldLeft(left)(z)(f), a)
        case Branch(Leaf(a), right) => foldLeft(right)(f(z, a))(f)
        case Branch(left, right) => {
          val acc = foldLeft(left)(z)(f)
          foldLeft(right)(acc)(f)
        }
      }

      def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {

        def go(t: Tree[A], acc: B): B = t match {
          case Leaf(a) => f(a, acc)
          case Branch(left, Leaf(a)) => go(left, f(a, acc))
          case Branch(Leaf(a), right) => go(right, f(a, acc))

          case Branch(left, right) => go(left, go(right, acc))
        }

        go(as, z)
      }
    }

  }

  def main(args: Array[String]): Unit = {

    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

    def suml(t: Tree[Int]): Int = Foldable[Tree].foldLeft(t)(0)(_ + _)

    def sumr(t: Tree[Int]): Int = Foldable[Tree].foldRight(t)(0)(_ + _)

    println(suml(tree))
    println(sumr(tree))
    println(suml(Leaf(1)))
    println(sumr(Leaf(1)))

  }

}
