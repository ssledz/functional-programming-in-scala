package pl.softech.learning.ch7

import java.util.concurrent.{ExecutorService, Executors}

import pl.softech.learning.ch7.Par.{Par, fork, map2, run, unit}

object Ex8 extends App {

  implicit val es = Executors.newSingleThreadExecutor()

  def eq[A](a: Par[A], b: Par[A])(implicit es: ExecutorService): Boolean = run(es)(a).get == run(es)(b).get

  def f(x: Int, y: Int): Int = x + y

  val x = 1


  println(eq(fork(map2(fork(unit(x)), fork(unit(x)))(f)), fork(unit(f(x, x)))))

}
