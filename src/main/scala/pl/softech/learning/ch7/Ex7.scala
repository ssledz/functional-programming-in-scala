package pl.softech.learning.ch7

import java.util.concurrent.{Executor, ExecutorService, Executors}

import pl.softech.learning.ch7.Ex4.map
import pl.softech.learning.ch7.Par.{Par, run, unit}

object Ex7 extends App {

  implicit val es = Executors.newFixedThreadPool(10)

  def eq[A](a: Par[A], b: Par[A])(implicit es : ExecutorService): Boolean = run(es)(a).get == run(es)(b).get

  def f(x: Int): Int = x + 1
  def g(x: Int): Int = x * 2

  val x = 1


  println(eq(map(unit(x))(f), unit(f(x))))

  val y = 1

  val h : Int => Int = f _ compose g _

  println(eq(map(map(unit(y))(g))(f), map(unit(y))(h)))

}
