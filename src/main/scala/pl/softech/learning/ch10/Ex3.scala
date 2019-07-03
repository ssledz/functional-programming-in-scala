package pl.softech.learning.ch10

object Ex3 {

  trait MonoidInstances {

    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {

      override def zero: A => A = (a : A) => a

      override def op(f: A => A, g: A => A): A => A = f compose g
    }

  }

  def main(args: Array[String]): Unit = {

    import Monoid._

    def f1(x : Int) = x + 2

    def f2(x : Int) = x * 2

    val arg = 3

    println(endoMonoid.op(f1, f2)(arg))
    println(endoMonoid.op(f1, endoMonoid.zero)(arg) + " == " + f1(arg))
    println(endoMonoid.op(endoMonoid.zero, f2)(arg) + " == " + f2(arg))

  }

}
