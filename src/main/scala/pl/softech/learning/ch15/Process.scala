package pl.softech.learning.ch15

import pl.softech.learning.ch13.IO
import pl.softech.learning.ch13.IO.IO
import pl.softech.learning.ch15.Process._

sealed trait Process[F[_], O] {

  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Halt(err) => Try(f(err))
    case Emit(head, tail) => Emit(head, tail.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: => Process[F, O]): Process[F, O] = onHalt {
    case End => p
    case err => Halt(err)
  }

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
    case Halt(err) => Halt(err)
    case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
    case Await(req, recv) => Await(req, recv andThen (_ flatMap f))
  }

  def onComplete(p: => Process[F, O]): Process[F, O] =
    onHalt {
      case End => p.asFinalizer
      case err => p.asFinalizer ++ Halt(err)
    }

  def asFinalizer: Process[F, O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e) => Halt(e)
    case Await(req, recv: (Either[Throwable, Any] => Process[F, O])) => await(req) {
      case Left(Kill) => this.asFinalizer
      case x: Either[Throwable, Any] => recv(x)
    }
  }

}

object Process extends Ex11.ProcessOps {

  def lines(filename: String): Process[IO, String] =
    resource(IO(io.Source.fromFile(filename))) { src =>
      lazy val iter = src.getLines // a stateful iterator
      def step: Option[String] = if (iter.hasNext) Some(iter.next) else None

      lazy val lines: Process[IO, String] = eval(IO(step)).flatMap {
        case None => Halt(End)
        case Some(line) => Emit(line, lines)
      }
      lines
    }(src => eval_(IO(src.close)))

  def resource[F[_], R, O](acquire: F[R])(use: R => Process[F, O])(release: R => Process[F, O]): Process[F, O] =
    await[F, R, O](acquire) {
      case Left(err) => Halt(err)
      case Right(r) => use(r).onComplete(release(r))
    }


  def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] = Await(req, recv)

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p
    catch {
      case e: Throwable => Halt(e)
    }

  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]

  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]

  case class Halt[F[_], O](err: Throwable) extends Process[F, O]

  case object End extends Throwable

  case object Kill extends Throwable

}
