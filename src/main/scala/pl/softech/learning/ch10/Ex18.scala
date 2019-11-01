package pl.softech.learning.ch10

import pl.softech.learning.ch10.Monoid._

object Ex18 {

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {

    override def zero: Map[K, V] = Map.empty

    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>

        val v = V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero))

        acc.updated(k, v)

      }
  }

  def monoid[A]: Monoid[Map[A, Int]] = mapMergeMonoid(Monoid.intAddition)

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {

    val xs = as.map(a => Map(a -> 1))

    foldMap[Map[A, Int], Map[A, Int]](xs.toList, monoid)(identity)

  }

  def main(args: Array[String]): Unit = {

    val res = bag(Vector("a", "rose", "is", "a", "rose"))

    println(res)

  }

}
