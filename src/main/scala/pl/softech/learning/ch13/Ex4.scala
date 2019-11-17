package pl.softech.learning.ch13

import java.util.concurrent.Executors

import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch13.NaturalTransformation.~>
import pl.softech.learning.ch7.parallelism.NonBlocking
import pl.softech.learning.ch7.parallelism.NonBlocking.Par

import scala.util.Try

object Ex4 {

  def main(args: Array[String]): Unit = {

    val program: Free[Console, Option[Int]] = for {
      _ <- Console.printLine("How old are you ?")
      age <- Console.readLine.map(_.flatMap(x => Try(x.toInt).toOption))
      _ <- age match {
        case Some(a) if a < 18 => Console.printLine(s"age: $a to low")
        case Some(a) => Console.printLine(s"You are $a age old")
        case None => Console.printLine(s"Illegal age: $age")
      }
    } yield age

    val consoleToFun0 = new (Console ~> Function0) {
      override def apply[A](f: Console[A]): () => A = f.toThunk
    }

    val consoleToPar = new (Console ~> Par) {
      override def apply[A](f: Console[A]): Par[A] = f.toPar
    }

    Free.run(program)(consoleToFun0)
    val p = Free.run(program)(consoleToPar)

    val es = Executors.newSingleThreadExecutor()
    val age = NonBlocking.run(es)(p)

    println(s"age: $age")
    es.shutdown()
  }

}
