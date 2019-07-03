package pl.softech.learning.ch10

object Ex2 {

  trait MonoidInstances {

    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {

      override def zero: Option[A] = None

      override def op(x: Option[A], y: Option[A]): Option[A] = x orElse y

    }
  }

  def main(args: Array[String]): Unit = {

    import Monoid._

    println(optionMonoid.op(Some(1), Some(2)))
    println(optionMonoid.op(Some(1), None))
    println(optionMonoid.op(None, None))

  }

}
