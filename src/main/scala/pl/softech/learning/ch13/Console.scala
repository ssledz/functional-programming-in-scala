package pl.softech.learning.ch13

import pl.softech.learning.ch7.parallelism.NonBlocking.{Par, _}

import scala.io.StdIn
import scala.util.Try

trait Console[A] {
  def toPar: Par[A]

  def toThunk: () => A
}

object Console {

  type ConsoleIO[A] = Free[Console, A]

  val readLine: ConsoleIO[Option[String]] = Free.Suspend(ReadLine)

  def printLine(s: String): ConsoleIO[Unit] = Free.Suspend(PrintLine(s))

  case object ReadLine extends Console[Option[String]] {
    def toPar: Par[Option[String]] = lazyUnit(run)

    def toThunk: () => Option[String] = () => run

    def run: Option[String] = Try(StdIn.readLine).toOption
  }

  case class PrintLine(line: String) extends Console[Unit] {

    override def toPar: Par[Unit] = lazyUnit(println(line))

    override def toThunk: () => Unit = () => println(line)
  }

}

