package pl.softech.learning.ch13

import java.util.concurrent.Executors

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch13.Async._
import pl.softech.learning.ch13.TailRec._
import pl.softech.learning.ch7.parallelism.NonBlocking
import pl.softech.learning.ch7.parallelism.NonBlocking.Par

object Ex0 {

  def main(args: Array[String]): Unit = {

    val F = Monad[TailRec]

    def f[A]: A => TailRec[A] = a => F.pure(a)

    val ff = List.fill(100000)(f[Int]).foldLeft(f[Int])(F.compose(_, _))

    println(ff(1))

    TailRec.run(ff(1)) === 1

    implicit val es = Executors.newFixedThreadPool(3)

    val runner = NonBlocking.run[Int](es) _

    val G = Monad[Async]

    def g[A]: A => Async[A] = a => G.pure(a)

    val gg = List.fill(100000)(g[Int]).foldLeft(g[Int])(G.compose(_, _))

    println(gg(1))

    val p: Par[Int] = Async.run(gg(1))

    runner(p) === 1

    val io = for {
      _ <- IO(println("Hello"))
      _ <- IO(println("World"))
    } yield ()

    IO.unsafePerformIO(io)

    println("shutting down")

    es.shutdown
  }

}
