package pl.softech.learning.ch10

import pl.softech.learning.ch10.Monoid._

object Ex7 {

  trait MonoidOps {

    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {

      if (v.length == 0) {
        m.zero
      } else if (v.length == 1) {
        f(v(0))
      } else {
        val (l, r) = v.splitAt(v.length / 2)
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
      }

    }

  }

  def main(args: Array[String]): Unit = {


    println(foldMapV(IndexedSeq("1", "2", "3"), Monoid.intAddition)(_.toInt))
    println(foldMapV(IndexedSeq("1", "2", "3", "4", "5"), Monoid.intAddition)(_.toInt))

  }

}
