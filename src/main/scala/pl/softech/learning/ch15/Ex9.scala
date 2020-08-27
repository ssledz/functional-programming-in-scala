package pl.softech.learning.ch15

import java.io.{File, FileWriter, Writer}

import pl.softech.learning.ch15.Process._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Ex9 {

  def processFile(input: File, output: File, process: Process[Double, Double]): Unit = {

    @tailrec
    def go(it: Iterator[String], out: Writer, p: Process[Double, Double]): Unit = {
      p match {
        case Emit(head, tail) =>
          out.write(s"$head\n")
          go(it, out, tail)
        case Await(recv) =>
          val next = if (it.hasNext) {
            Try(it.next().toDouble) match {
              case Failure(_) => p
              case Success(value) => recv(Some(value))
            }
          } else {
            recv(None)
          }
          go(it, out, next)
        case Halt() =>
      }

    }

    val s = io.Source.fromFile(input)
    val o = new FileWriter(output)
    try go(s.getLines(), o, process)
    finally {
      Try(s.close())
      o.close()
    }
  }

  def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

  def main(args: Array[String]): Unit = {
    val in = getClass.getClassLoader.getResource("fahrenheit.txt")
    val p = lift(toCelsius)
    processFile(new File(in.getFile), new File("/tmp/celsius.txt"), p)
  }

}
