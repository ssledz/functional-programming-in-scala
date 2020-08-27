package pl.softech.learning.ch15

import pl.softech.learning.Assertion._
import pl.softech.learning.ch15.Process1._

object Ex6 {

  trait ProcessOps {
    def zipWithIndex[I]: Process1[I, (I, Int)] = {
      def go(acc: Int): Process1[I, (I, Int)] = Await {
        case Some(x) => Emit((x, acc), go(acc + 1))
        case None => Halt()
      }

      go(0)
    }

    implicit class ZipOps[I, O](val p: Process1[I, O]) {
      def zipWithIndex: Process1[I, (O, Int)] = p |> Process1.zipWithIndex
    }

  }

  def main(args: Array[String]): Unit = {

    zipWithIndex(Stream("a", "b")).toList === List(("a", 0), ("b", 1))

    val appendX = lift[String, String](_ + "x")

    appendX.zipWithIndex(Stream("a", "b")).toList === List(("ax", 0), ("bx", 1))

  }

}
