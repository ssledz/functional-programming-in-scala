package pl.softech.learning.ch12

import pl.softech.learning.Assertion._
import pl.softech.learning.ch12.TraverseInstances._

object Ex17 {

  def foldLeft[A, B, F[_] : Traverse](fa: F[A])(z: B)(f: (B, A) => B): B =
    Traverse[F].mapAccum(fa, z)((x, acc) => ((), f(acc, x)))._2

  def main(args: Array[String]): Unit = {

    val xs = List(1, 2, 3, 4, 5)

    foldLeft(xs)(List.empty[Int])((ys, y) => y :: ys) === xs.reverse

    foldLeft(Option(1))(0)(_ + _) === 1

    foldLeft(None: Option[Int])(0)(_ + _) === 0

  }

}
