package pl.softech.learning.ch10

trait Monoid[A] {

  self =>

  def zero: A

  def op(a1: A, a2: A): A

  def dual: Monoid[A] = new Monoid[A] {

    def zero: A = self.zero

    def op(a1: A, a2: A): A = self.op(a2, a1)
  }

}

trait MonoidInstances {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

}

object Monoid extends MonoidInstances with Ex1.MonoidInstances with Ex2.MonoidInstances
  with Ex3.MonoidInstances with Ex5.MonoidOps with Ex6.MonoidOps with Ex7.MonoidOps with Ex8.MonoidOps
