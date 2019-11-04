package pl.softech.learning.ch11

import java.util.concurrent.Executors

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch11.MonadSyntax._
import pl.softech.learning.ch7.parallelism.NonBlocking
import pl.softech.learning.ch7.parallelism.NonBlocking.Par

object Ex1 {

  trait MonadInstances {

    implicit val listMonadInstance: Monad[List] = new Monad[List] {
      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

      def pure[A](a: A): List[A] = List(a)
    }

    implicit val optionMonadInstance: Monad[Option] = new Monad[Option] {
      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

      def pure[A](a: A): Option[A] = Option(a)
    }

    implicit val parMonadInstance: Monad[Par] = new Monad[Par] {
      def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = NonBlocking.flatMap(fa)(f)

      def pure[A](a: A): Par[A] = NonBlocking.unit(a)
    }

    implicit val streamMonadInstance: Monad[Stream] = new Monad[Stream] {
      def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa.flatMap(f)

      def pure[A](a: A): Stream[A] = Stream(a)
    }

  }

  def main(args: Array[String]): Unit = {

    val parMonad = Monad[Par]

    val ec = Executors.newFixedThreadPool(3)

    val sum = for {
      a <- NonBlocking.lazyUnit {
        println(1)
        1
      }
      b <- NonBlocking.lazyUnit {
        println(2)
        2
      }
    } yield a + b

    NonBlocking.run(ec)(sum) === 3

    println("end...")

    ec.shutdown()

  }

}
