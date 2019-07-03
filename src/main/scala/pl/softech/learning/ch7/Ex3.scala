package pl.softech.learning.ch7

import java.util.concurrent._

object Ex3 extends App {

  object Par {

    type Par[A] = ExecutorService => Future[A]

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def unit[A](a: A): Par[A] = _ => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
      val ar = a(es)
      val br = b(es)
      UnitFuture(f(ar.get, br.get))
    }

    def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get)

  }

  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  val es = Executors.newFixedThreadPool(32)
  val runner = Par.run[Int](es) _

  println(runner(sum(1 to 115)).get(1000, TimeUnit.MILLISECONDS))

}
