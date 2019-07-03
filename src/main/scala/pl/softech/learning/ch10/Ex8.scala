package pl.softech.learning.ch10

import pl.softech.learning.ch10.Monoid._
import pl.softech.learning.ch7.Par.Par

object Ex8 {

  trait MonoidOps {

    def par[A](m: Monoid[A]): Monoid[Par[A]] = ???

    def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = ???

  }

  def main(args: Array[String]): Unit = {


    println(parFoldMap(IndexedSeq("1", "2", "3"), Monoid.intAddition)(_.toInt))
    println(parFoldMap(IndexedSeq("1", "2", "3", "4", "5"), Monoid.intAddition)(_.toInt))

  }

}
