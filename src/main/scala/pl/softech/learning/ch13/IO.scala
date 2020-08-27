package pl.softech.learning.ch13

import java.util.concurrent.ExecutorService

import pl.softech.learning.ch11.MonadInstances._
import pl.softech.learning.ch13.NaturalTransformation.~>
import pl.softech.learning.ch7.parallelism.NonBlocking
import pl.softech.learning.ch7.parallelism.NonBlocking.Par

object IO {

  type IO[A] = Free[Par, A]

  def apply[A](a: => A): IO[A] = Free.pure(()).map(_ => a)

  private def identity[G[_]]: G ~> G = new (G ~> G) {
    def apply[A](f: G[A]): G[A] = f
  }

  def unsafePerformIO[A](io: IO[A])(implicit es: ExecutorService): A = {
    val runner = NonBlocking.run[A](es) _
    runner(Free.run(io)(identity[Par]))
  }
}
