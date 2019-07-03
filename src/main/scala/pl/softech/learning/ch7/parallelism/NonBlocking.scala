package pl.softech.learning.ch7.parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object NonBlocking {

  sealed trait Future[A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
    def call = r
  })

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    p(es) { a => ref.set(a); latch.countDown }

    latch.await

    ref.get
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
    def apply(cb: C => Unit): Unit = {
      var ar: Option[A] = None
      var br: Option[B] = None

      val combiner = Actor[Either[A, B]](es) {

        case Left(a) => br match {
          case None => ar = Some(a)
          case Some(b) => eval(es)(cb(f(a, b)))
        }

        case Right(b) => ar match {
          case None => br = Some(b)
          case Some(a) => eval(es)(cb(f(a, b)))
        }

      }

      pa(es)(a => combiner ! Left(a))
      pb(es)(b => combiner ! Right(b))
      
    }


  }

}
