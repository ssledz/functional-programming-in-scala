package pl.softech.learning.ch10

import pl.softech.learning.ch10.FoldableInstances._

object Ex14 {

  trait FoldableInstances {

    implicit val foldableOptionInstance: Foldable[Option] = new Foldable[Option] {

      private def dual[A, B, C](f: (A, B) => C) = (b: B, a: A) =>
        f(a, b)

      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
        as.map(f(_, z)).getOrElse(z)

      override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
        foldRight(as)(z)(dual(f))
    }

  }

  def main(args: Array[String]): Unit = {

    println(Foldable[Option].foldLeft(Some(1))(1)(_ + _))
    println(Foldable[Option].foldLeft(None: Option[Int])(1)(_ + _))
    println(Foldable[Option].foldRight(None: Option[Int])(1)(_ + _))

  }

}
