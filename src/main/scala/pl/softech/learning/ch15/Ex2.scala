package pl.softech.learning.ch15

import pl.softech.learning.Assertion._
import pl.softech.learning.ch15.Process1._

object Ex2 {

  trait ProcessOps {

    def count[I]: Process1[I, Int] = {
      def go(acc: Int): Process1[I, Int] = Await {
        case Some(_) => Emit(acc, go(acc + 1))
        case None => Halt()
      }

      go(1)

    }

  }

  def main(args: Array[String]): Unit = {

    count(Stream("a", "b", "c")).toList === List(1, 2, 3)

    count(Stream.empty).toList === List.empty

  }

}
