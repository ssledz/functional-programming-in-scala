package pl.softech.learning.ch7

import java.util.concurrent.Executors

import pl.softech.learning.ch7.Ex4._
import pl.softech.learning.ch7.Par._

object Ex5 extends App {

  def sequence[A](xs: List[Par[A]]): Par[List[A]] = es => xs match {
    case h :: t => map2(h, sequence(t))(_ :: _)(es)
    case _ => unit(List.empty[A])(es)
  }

  val res = parMap(List(1, 2, 3, 4)) {
    x => {
      Thread.sleep(x * 1000)
      x * 2
    }
  }

  def doRun(res: Par[List[Int]])(nThreads: Int): Unit = {
    val start = System.currentTimeMillis()
    println("Start with " + nThreads + " threads")
    println(Par.run(Executors.newFixedThreadPool(nThreads))(res))
    println("end after: " + (System.currentTimeMillis() - start) + " ms")
  }

  val example = doRun(res) _

  example(4)
  example(1)

}
