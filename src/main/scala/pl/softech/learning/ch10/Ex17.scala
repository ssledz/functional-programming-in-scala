package pl.softech.learning.ch10

import pl.softech.learning.ch10.Monoid._

object Ex17 {

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {

    override def zero: A => B = (a: A) => B.zero

    override def op(a1: A => B, a2: A => B): A => B = (a: A) => B.op(a1(a), a2(a))

  }

  def main(args: Array[String]): Unit = {

    val intMonoid = Monoid.intAddition

    val fx0: String => Int = _ => 1
    val fx1: String => Int = _.toInt * 2
    val fx2: String => Int = x => Math.pow(x.toInt, 2).toInt

    val m: Monoid[String => Int] = functionMonoid(intMonoid)

    // x^2 + 2x + 1
    val fx = foldMap(List(fx0, fx1, fx2), m)(identity)

    for (x <- -2 to 2) {
      println(s"x=$x y=${fx(x.toString)}")
    }
  }

}
