package pl.softech.learning.ch10

object Ex1 {

  trait MonoidInstances {

    val intAddition: Monoid[Int] = new Monoid[Int] {
      override def zero: Int = 0

      override def op(a1: Int, a2: Int): Int = a1 + a2
    }
    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      override def zero: Int = 1

      override def op(a1: Int, a2: Int): Int = a1 * a2
    }
    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      override def zero: Boolean = false

      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    }
    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      override def zero: Boolean = true

      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    }

  }

  def main(args: Array[String]): Unit = {

    import Monoid._

    println(intAddition.op(1, 2))
    println(intMultiplication.op(1, 2))
    println(booleanAnd.op(true, false))
    println(booleanOr.op(true, false))

  }

}
