package pl.softech.learning.ch7

import java.util.concurrent.Executors

import pl.softech.learning.ch7.Ex2.Par.Par
import pl.softech.learning.ch7.Ex4._
import pl.softech.learning.ch7.Ex5._

object Ex6 extends App {

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    def helper(x: A): Option[A] = if (f(x)) Some(x) else None

    val xx: List[Par[Option[A]]] = as.map(asyncF(helper))
    val yy: Par[List[Option[A]]] = sequence(xx)
    map(yy)(_.flatten)
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    val g: Par[C => D] = Par.map2(a, b)((aa, bb) => f(aa, bb, _))
    Par.map2(g, c)(_ (_))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val g: Par[(C, D) => E] = Par.map2(a, b)((aa, bb) => f(aa, bb, _, _))
    val h: Par[D => E] = Par.map2(g, c)((gg, cc) => gg(cc, _))
    Par.map2(h, d)(_ (_))
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    val g: Par[E => F] = map4(a, b, c, d)((aa, bb, cc, dd) => f(aa, bb, cc, dd, _))
    Par.map2(g, e)(_ (_))
  }

  def reduce[A, B](xs: Seq[A])(z: B)(m: A => B)(f: (B, B) => B): Par[B] =
    if (xs.size <= 1)
      Par.unit(xs.headOption.map(m) getOrElse z)
    else {
      val (l, r) = xs.splitAt(xs.length / 2)
      Par.map2(Par.fork(reduce(l)(z)(m)(f)), Par.fork(reduce(r)(z)(m)(f)))(f)
    }

  def sum(xs: Seq[Int]): Par[Int] = reduce(xs)(0)(identity)(_ + _)

  val xs = Seq(1, 2, 3, 4, 5)

  println("xs: " + xs)

  val s = sum(xs)

  val runner = Par.run[Int](Executors.newFixedThreadPool(8)) _

  println("sum: " + runner(s))

  def max(xs: Seq[Int]): Par[Int] = reduce(xs)(0)(identity)(Math.max)

  val m = max(xs)

  println("max: " + runner(m))

  def words(s: String): Int = s.split(' ').length

  def sumWords(xs: List[String]): Par[Int] = reduce(xs)(0)(words(_))(_ + _)

  val txt = List(
    "Write a function that takes a list",
    "of paragraphs (a List[String] )",
    "and returns\nthe total number of words across",
    "all paragraphs, in parallel.",
    "Generalize this\nfunction as much as possible"
  )

  val words = sumWords(txt)

  println("words: " + runner(words))
}
