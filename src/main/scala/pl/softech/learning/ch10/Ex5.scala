package pl.softech.learning.ch10

import pl.softech.learning.ch10.Monoid._

object Ex5 {

  trait MonoidOps {

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.foldLeft(m.zero) { (b, a) =>
        m.op(f(a), b)
      }

  }

  def main(args: Array[String]): Unit = {

    println(foldMap(List("1", "2", "3"), Monoid.intAddition)(_.toInt))


  }

}
