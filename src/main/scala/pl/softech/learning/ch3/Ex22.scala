package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

import scala.annotation.tailrec

object Ex22 {

  def add2(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Cons(x, txs), Cons(y, tys)) => Cons(x + y, add2(txs, tys))
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }

  def add(xs: List[Int], ys: List[Int]): List[Int] = {
    @tailrec
    def add(xs: List[Int], ys: List[Int], acc: List[Int]): List[Int] = (xs, ys) match {
      case (Cons(x, tl), Cons(y, tr)) => add(tl, tr, Cons(x + y, acc))
      case (Nil, _) => acc
      case (_, Nil) => acc
    }

    add(xs, ys, Nil).revers
  }

  def main(args: Array[String]): Unit = {
    println(add(List(1, 2, 3), List(1, 2)))
    println(add2(List(1, 2, 3), List(1, 2)))
  }

}
