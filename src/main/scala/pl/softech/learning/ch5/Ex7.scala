package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex7 {

  def map[A, B](s: Stream[A])(f: A => B): Stream[B] = s.foldRight(Stream.empty[B]) {
    (a, acc) => Stream.cons(f(a), acc)
  }

  def concat[A](xs: Stream[A], ys: Stream[A]): Stream[A] = xs.foldRight(ys){
    (a, acc) => Stream.cons(a, acc)
  }

  def flatMap[A, B](s: Stream[A])(f: A => Stream[B]): Stream[B] = s.foldRight(Stream.empty[B]) {
    (a, acc) => concat(f(a), acc)
  }

  def filter[A](s: Stream[A])(p: A => Boolean): Stream[A] = s.foldRight(Stream.empty[A]) {
    (a, acc) => if(p(a)) Stream.cons(a, acc) else acc
  }

  def append[A](s: Stream[A], a: A): Stream[A] = concat(s, Stream.cons(a, Stream.empty))

  trait Implicits {

    implicit class Ex7Opts[A](s: Stream[A]) {

      def map[B](f: A => B): Stream[B] = Ex7.map(s)(f)

      def concat(ys: Stream[A]): Stream[A] = Ex7.concat(s, ys)

      def flatMap[B](f: A => Stream[B]): Stream[B] = Ex7.flatMap(s)(f)

      def filter(p: A => Boolean): Stream[A] = Ex7.filter(s)(p)

      def append(a: A): Stream[A] = Ex7.append(s, a)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).map(_ + 111).toList)
    println(Stream(1, 2, 3, 4).flatMap(e => Stream.cons(e, Stream.cons(e, Empty))).toList)
    println(Stream(1, 2, 3, 4).append(5).append(6).toList)
    println(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList)
  }

}
