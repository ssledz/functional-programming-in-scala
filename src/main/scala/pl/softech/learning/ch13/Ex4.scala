package pl.softech.learning.ch13

import java.util.concurrent.Executors

import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch13.NaturalTransformation.~>
import pl.softech.learning.ch7.parallelism.NonBlocking
import pl.softech.learning.ch7.parallelism.NonBlocking.Par

import scala.util.Try

object Ex4 {

  val consoleToFun0 = new (Console ~> Function0) {
    override def apply[A](f: Console[A]): () => A = f.toThunk
  }

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {

    type FreeG[A] = Free[G, A]

    val t = new (F ~> FreeG) {
      override def apply[A](f: F[A]): FreeG[A] = Free.Suspend(fg(f))
    }

    Free.run(f)(t)
  }

  //  def runConsole[A](a: Free[Console, A]): A = {
  //    Ex2.runTrampoline {
  //      translate(a)(consoleToFun0)
  //    }
  //  }

  def runConsole[A](a: Free[Console, A]): A = {

    val identityT = new (Function0 ~> Function0) {
      def apply[A](f: () => A): () => A = f
    }

    val res = Free.run(translate(a)(consoleToFun0))(identityT)

    res()

  }

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


    val consoleToPar = new (Console ~> Par) {
      override def apply[A](f: Console[A]): Par[A] = f.toPar
    }

    //    Free.run(program)(consoleToFun0)

    val es = Executors.newSingleThreadExecutor()

    val runner = NonBlocking.run[Option[Int]](es)

    runner(Free.run(program)(consoleToPar))

    es.shutdown()

    runConsole(program)
  }

}
