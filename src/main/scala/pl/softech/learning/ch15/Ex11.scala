package pl.softech.learning.ch15

import java.util.concurrent.{ExecutorService, Executors}

import pl.softech.learning.Assertion._
import pl.softech.learning.ch13.IO
import pl.softech.learning.ch13.IO.IO
import pl.softech.learning.ch15.Process._

object Ex11 {

  trait ProcessOps {

    def eval[F[_], A](fa: F[A]): Process[F, A] = await(fa) {
      case Right(value) => Emit(value, Halt(End))
      case Left(err) => Halt(err)
    }

    def eval_[F[_], A, B](fa: F[A]): Process[F, B] = await(fa)(_ => Halt(End))

  }

  def main(args: Array[String]): Unit = {


    implicit val es: ExecutorService = Executors.newSingleThreadExecutor()
    val file = getClass.getClassLoader.getResource("fahrenheit.txt").getFile
    val task = runLog(lines(file))
    val xs = IO.unsafePerformIO(task)

    xs.head === "# Moderate Oven"

    IO.unsafePerformIO(MonadCatch[IO].attempt(IO(throw new RuntimeException("err1")))).left.map(_.getMessage) === Left("err1")
    IO.unsafePerformIO(MonadCatch[IO].attempt(IO.defer(IO(throw new RuntimeException("err2"))))).left.map(_.getMessage) === Left("err2")

    es.shutdown()


  }

}
