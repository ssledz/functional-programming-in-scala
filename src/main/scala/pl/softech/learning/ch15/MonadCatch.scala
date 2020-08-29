package pl.softech.learning.ch15

import pl.softech.learning.ch11.Monad
import pl.softech.learning.ch13.Free.{FlatMap, Suspend}
import pl.softech.learning.ch13.IO
import pl.softech.learning.ch13.IO.IO

import scala.util.{Failure, Success, Try}

trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable, A]]

  def fail[A](t: Throwable): F[A]
}

object MonadCatch {

  def apply[F[_]](implicit F: MonadCatch[F]): MonadCatch[F] = F

  implicit val monadTryCatchInstance: MonadCatch[Try] = new MonadCatch[Try] {

    override def attempt[A](a: Try[A]): Try[Either[Throwable, A]] = a match {
      case Failure(exception) => Success(Left(exception))
      case Success(value) => Success(Right(value))
    }

    override def fail[A](t: Throwable): Try[A] = Failure(t)

    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

    override def pure[A](a: A): Try[A] = Success(a)
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


}
