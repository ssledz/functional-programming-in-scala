package pl.softech.learning.ch3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A] {

  override def toString: String = {

    def toString(l: List[A], acc: String): String = l match {
      case Cons(h, t) => toString(t, s"$acc,$h")
      case Nil => acc
    }

    toString(tail, s"[$head") + "]"

  }
}

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  object Implicits extends Ex2.Implicits with Ex3.Implicits with Ex4.Implicits with Ex5.Implicits
    with Ex6.Implicits with Ex7.Implicits with Ex9.Implicits with Ex10.Implicits
    with Ex12.Implicits with Ex14.Implicits with Ex15.Implicits with Ex18.Implicits
    with Ex19.Implicits with Ex20.Implicits with Ex23.Implicits with Ex24.Implicits

}
