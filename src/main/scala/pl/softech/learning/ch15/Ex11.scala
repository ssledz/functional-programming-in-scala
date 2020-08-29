package pl.softech.learning.ch15

import java.util.concurrent.{ExecutorService, Executors}

import pl.softech.learning.Assertion._
import pl.softech.learning.ch13.Free.{FlatMap, Suspend}
import pl.softech.learning.ch13.IO
import pl.softech.learning.ch13.IO.IO
import pl.softech.learning.ch15.Ex10._
import pl.softech.learning.ch15.Process._

import scala.util.{Failure, Success}

object Ex11 {

  trait ProcessOps {

    def eval[F[_], A](fa: F[A]): Process[F, A] = await(fa) {
      case Right(value) => Emit(value, Halt(End))
      case Left(err) => Halt(err)
    }

    def eval_[F[_], A, B](fa: F[A]): Process[F, B] = await(fa)(_ => Halt(End))

  }

  implicit val ioCatchInstance: MonadCatch[IO] = new MonadCatch[IO] {

    def safe[A](f: Any => IO[A]): Any => IO[Either[Throwable, A]] = a => IO(()).flatMap { _ =>
      scala.util.Try(f(a)) match {
        case Failure(exception) => IO.pure(Left(exception))
        case Success(value) => attempt(value)
      }
    }

    override def attempt[A](a: IO[A]): IO[Either[Throwable, A]] = a match {
      case FlatMap(fa: IO[_], f: (Any => IO[A])) => FlatMap(fa, safe(f))
      case Suspend(_) => attempt(FlatMap(IO.pure(()), (x: Unit) => a))
      case _ => a.map(Right(_))
    }

    override def fail[A](t: Throwable): IO[A] = IO(throw t)

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

    override def pure[A](a: A): IO[A] = IO.pure(a)
  }

  def main(args: Array[String]): Unit = {


    implicit val es: ExecutorService = Executors.newSingleThreadExecutor()
    val file = getClass.getClassLoader.getResource("fahrenheit.txt").getFile
    val task = runLog(lines(file))
    val xs = IO.unsafePerformIO(task)

    xs.head === "# Moderate Oven"

    IO.unsafePerformIO(ioCatchInstance.attempt(IO(throw new RuntimeException("err1")))).left.map(_.getMessage) === Left("err1")
    IO.unsafePerformIO(ioCatchInstance.attempt(IO.defer(IO(throw new RuntimeException("err2"))))).left.map(_.getMessage) === Left("err2")

    es.shutdown()


  }

}
