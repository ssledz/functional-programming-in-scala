package pl.softech.learning.ch15

import pl.softech.learning.Assertion._
import pl.softech.learning.ch15.Process1._

object Ex3 {

  trait ProcessOps {

    def mean: Process1[Double, Double] = {
      def go(acc: Double, cnt: Int): Process1[Double, Double] = Await {
        case Some(x) =>
          val newAcc = acc + x
          val newCnt = cnt + 1
          Emit(newAcc / newCnt, go(newAcc, newCnt))
        case None => Halt()
      }

      go(0, 0)
    }

  }

  def main(args: Array[String]): Unit = {
    mean(Stream(1, 2, 3, 4, 5, 6)).toList === List(1.0, 1.5, 2.0, 2.5, 3.0, 3.5)
  }

}
