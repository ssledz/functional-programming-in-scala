package pl.softech.learning.ch10

object Ex6 {

  trait MonoidOps {

    def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {

      override def zero: A = m.zero

      override def op(a1: A, a2: A): A = m.op(a2, a1)
    }

  }


  import Monoid._

  def foldRight[A, B](xs: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(xs, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](xs: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(xs, dual(endoMonoid[B]))(a => f(_, a))(z)

  def main(args: Array[String]): Unit = {

    val xs = List("1", "2", "3")

    def f(x: String, y: Int): Int = x.toInt + y

    def g(x: Int, y: String): Int = x - y.toInt

    println(foldLeft(xs)(0)(g))
    println(xs.foldLeft(0)(g))
    println(foldRight(xs)(0)(f))
    println(xs.foldRight(0)(f))
  }

}
