package pl.softech.learning.ch2

import scala.annotation.tailrec

object Ex2 {

  def binarySearch[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key, a)) mid2
        else if (greater) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }

    go(0, 0, as.length - 1)
  }

  def isSorted[@specialized A](xs: Array[A], gt: (A, A) => Boolean): Boolean = {

    val ub = xs.size - 1

    @tailrec
    def isSorted[A](i: Int, acc: Boolean): Boolean = {

      if (!acc) false
      else if (i == ub) acc
      else isSorted(i + 1, !gt(xs(i), xs(i + 1)))

    }

    isSorted(0, true)


  }


  def main(args: Array[String]): Unit = {

    val xs = Array(12, 1, 22, 2, 0)
    println(isSorted[Int](xs, _ > _))
    println(isSorted[Int](xs.sorted, _ > _))


  }

}
