package pl.softech.learning.ch15

import pl.softech.learning.Assertion._
import pl.softech.learning.ch15.Process1._

object Ex8 {

  trait ProcessOps {

    def exists[I](f: I => Boolean): Process1[I, Boolean] = Await {
      case Some(value) => if (f(value)) Emit(true) else exists(f)
      case None => Emit(false)
    }

  }

  def main(args: Array[String]): Unit = {

    val p = exists[Int](_ % 2 == 0)

    p(Stream(1, 2, 3, 4, 5)).toList === List(true)

    p(Stream(1, 3, 5)).toList === List(false)

  }

}
