package pl.softech.learning.ch10

import pl.softech.learning.ch10.Monoid._

import scala.math.Ordering
import scala.util.Random

/**
  * Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll need
  * to come up with a creative Monoid .
  */
object Ex9 {


  def main(args: Array[String]): Unit = {

    def isOrdered[A: Ordering](xs: IndexedSeq[A]): Boolean = {

      import Ordered._

      type AUX = (Boolean, A)

      val f: A => AUX => AUX = (y: A) => (px: AUX) => px match {
        case (acc, x) => (acc && (y >= x), y)
      }

      val m: Monoid[AUX => AUX] = dual(endoMonoid[AUX])

      val res = foldMapV(xs, m)(f)

      res(true, xs.head)._1

    }

    def genInt(n: Int): IndexedSeq[Int] = IndexedSeq.fill(n)(Random.nextInt(100))

    for (i <- 1 to 10) {

      val notOrdered = genInt(10)
      val ordered = genInt(10).sorted

      println(s"$ordered is ordered: " + isOrdered(ordered))
      println(s"$notOrdered is ordered: " + isOrdered(notOrdered))

    }

  }

}
