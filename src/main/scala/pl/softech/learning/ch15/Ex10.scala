package pl.softech.learning.ch15

import java.io.{BufferedReader, File, FileReader}

import pl.softech.learning.Assertion._
import pl.softech.learning.ch11.MonadSyntax._
import pl.softech.learning.ch15.Process._

import scala.util.{Success, Try}

object Ex10 {

  trait ProcessOps {

    def runLog[F[_], O](src: Process[F, O])(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
      def go(fCurr: F[Process[F, O]], acc: IndexedSeq[O]): F[IndexedSeq[O]] = fCurr.flatMap {
        case Emit(head, tail) => go(F.pure(tail), acc :+ head)
        case Halt(End) => F.pure(acc)
        case Halt(err) => F.fail(err)
        case Await(req: F[Any], recv: (Either[Throwable, Any] => Process[F, O])) =>
          go(F.attempt(req).map(x => recv(x)), acc)
      }

      go(F.pure(src), IndexedSeq.empty)
    }
  }

  def main(args: Array[String]): Unit = {

    def add[F[_] : MonadCatch](x: String, y: String): F[Int] = for {
      a <- MonadCatch[F].pure(x).map(_.toInt)
      b <- MonadCatch[F].attempt(MonadCatch[F].pure(y).map(_.toInt)).flatMap {
        case Left(_) => MonadCatch[F].pure(0)
        case Right(value) => MonadCatch[F].pure(value)
      }
    } yield a + b

    add[Try]("1", "2") === Success(3)
    add[Try]("1", "a") === Success(1)

    val p: Process[Try, String] = {
      val in = getClass.getClassLoader.getResource("fahrenheit.txt")
      lazy val next: Process[Try, String] = await(Try(new BufferedReader(new FileReader(new File(in.getFile))))) {
        case Right(b) =>
          await(Try(b.readLine())) {
            case Right(value) => if (value == null) Halt(End) else Emit(value, next)
            case Left(err) => await(Try(b.close()))(_ => Halt(err))
          }
        case Left(err) => Halt(err)
      }
      next
    }

    val xs: Try[IndexedSeq[String]] = runLog(p)

    xs.get.head === "# Moderate Oven"

  }

}
