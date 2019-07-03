package pl.softech.learning.ch5

import pl.softech.learning.ch5.Ex11.unfold
import pl.softech.learning.ch5.Stream.Implicits._

object Ex13 {

  def map[A, B](s: Stream[A])(f: A => B): Stream[B] = unfold(s) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def take[A](s: Stream[A], n: Int): Stream[A] = unfold((s, n)) {
    case (Cons(h, t), c) if c > 0 => Some(h(), (t(), c - 1))
    case _ => None
  }

  def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] = unfold(s) {
    case Cons(h, t) if (p(h())) => Some((h(), t()))
    case _ => None
  }

  def zipWith[A, B, C](xs: Stream[A], ys: Stream[B])(f: (A, B) => C): Stream[C] = unfold((xs, ys)) {
    case (Cons(xh, xt), Cons(yh, yt)) => Some((f(xh(), yh()), (xt(), yt())))
    case _ => None
  }

  def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((s1, s2)) {
    case (Cons(xh, xt), Cons(yh, yt)) => Some(((Some(xh()), Some(yh())), (xt(), yt())))
    case (s@Empty, Cons(yh, yt)) => Some(((None, Some(yh())), (s, yt())))
    case (Cons(xh, xt), s@Empty) => Some(((Some(xh()), None), (xt(), s)))
    case _ => None
  }

  trait Implicits {

    implicit class Ex13Opts[A](s: Stream[A]) {

      def zipWith(ys: Stream[A])(f: (A, A) => A): Stream[A] = Ex13.zipWith(s, ys)(f)

      def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Ex13.zipAll(s, s2)

    }

  }

  def main(args: Array[String]): Unit = {
    println(map(Stream(1, 2, 3, 4))(_ + 2).toList)
    println(take(Stream(1, 2, 3, 4), 2).toList)
    println(takeWhile(Stream(1, 2, 3, 4))(_ < 4).toList)
    println(Stream(1, 2, 3, 4).zipWith(Stream(2, 3, 4))(_ + _).toList)
    println(Stream(1, 2, 3, 4).zipAll(Stream(2, 3, 4)).toList)
  }

}
