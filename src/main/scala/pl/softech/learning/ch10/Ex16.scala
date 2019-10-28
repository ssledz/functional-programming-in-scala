package pl.softech.learning.ch10

import scala.util.Random

object Ex16 {

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {

    override def zero: (A, B) = (a.zero, b.zero)

    override def op(a1: (A, B), a2: (A, B)): (A, B) = (a1, a2) match {
      case ((a1a, a1b), (a2a, a2b)) => (a.op(a1a, a2a), b.op(a1b, a2b))
    }
  }

  def assertLaws[A](m: Monoid[A])(a1: A, a2: A, a3: A): Unit = {

    assert(m.op(m.zero, m.zero) == m.zero)
    assert(m.op(m.zero, a1) == a1)
    assert(m.op(a1, m.zero) == a1)
    assert(m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3)))

  }

  def main(args: Array[String]): Unit = {

    val sumMonoid = new Monoid[Int] {
      override def zero: Int = 0

      override def op(a1: Int, a2: Int): Int = a1 + a2
    }

    val prodMonoid = productMonoid(sumMonoid, sumMonoid)

    import Monoid._

    val l: List[(Int, Int)] = (1 to 5).map((1, _)).toList


    val x = foldMap(l, prodMonoid)(identity)
    println(l.map(_._2))
    println("sum: " + x._2)
    println("count: " + x._1)
    println("average: " + (x._2.toDouble / x._1))

    def nextInt = Random.nextInt(100)

    def nextIntPair = (nextInt, nextInt)

    for (1 <- 1 to 100) {

      assertLaws(sumMonoid)(nextInt, nextInt, nextInt)
      assertLaws(prodMonoid)(nextIntPair, nextIntPair, nextIntPair)


    }


  }

}
