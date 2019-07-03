package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

object Ex24 {
  
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =
    subs(l).filter(_ == sub) match {
      case Cons(_, _) => true
      case Nil => false
    }

  def inits[A](l: List[A], acc: List[List[A]]): List[List[A]] = l match {
    case Nil => acc.tail
    case _ => {
      val init = l.init
      inits(init, Cons(init, acc))
    }
  }

  def tails[A](l: List[A], acc: List[List[A]]): List[List[A]] = l match {
    case Cons(head, tail) => tails(tail, Cons(tail, acc))
    case Nil => acc.tail
  }

  def subs[A](l: List[A]): List[List[A]] = {
    val t = tails(l, Nil)
    val i = inits(l, Nil)
    val o = t.flatMap(v => inits(v, Nil))
    t ++ i ++ o append l
  }

  def eq[A](xs: List[A], ys: List[A]): Boolean = (xs, ys) match {
    case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) eq(t1, t2) else false
    case (Nil, Nil) => true
    case (_, Nil) => false
    case (Nil, _) => false
  }

  trait Implicits {

    implicit class Ex24ListOpts[A](l: List[A]) {
      def hasSubsequence(sub: List[A]): Boolean = Ex24.hasSubsequence(l, sub)

      def ==(ys: List[A]): Boolean = Ex24.eq(l, ys)
    }

  }

  def main(args: Array[String]): Unit = {
    println(tails(List(1, 2, 3, 4), Nil))
    println(inits(List(1, 2, 3, 4), Nil))
    println(inits(List(1), Nil))
    println(subs(List(1, 2, 3, 4)))
    println(List(1, 2, 3, 4, 5, 6).hasSubsequence(List(2, 3, 4)))
    println(List(1, 2, 3, 4, 5, 6).hasSubsequence(List(5)))
    println(List(1, 2, 3, 4, 5, 6).hasSubsequence(List(1, 1, 2)))
    println(List(1, 2, 3).hasSubsequence(List(1, 2, 3)))
    println(List(1, 2, 3).hasSubsequence(List(1, 2, 3, 4)))
  }

}
