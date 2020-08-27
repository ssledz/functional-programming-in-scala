package pl.softech.learning.ch15

import pl.softech.learning.Assertion._
import pl.softech.learning.ch15.Process1._


object Ex0 {

  def main(args: Array[String]): Unit = {

    val units: Stream[Unit] = Stream.continually(())

    val ones: Process1[Unit, Int] = lift((_: Unit) => 1)

    ones(units).take(3).toList === List(1, 1, 1)

    val even: Process1[Int, Int] = filter((x: Int) => x % 2 == 0)

    even(Stream(1, 2, 3, 4)).toList === List(2, 4)

    sum(Stream(1.0, 2.0, 3.0, 4.0)).toList === List(1.0, 3.0, 6.0, 10.0)

  }

}
