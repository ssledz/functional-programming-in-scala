package pl.softech.learning.ch11

case class Reader[R, A](run: R => A) {

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader { r =>
    f(run(r)).run(r)
  }

}

object Reader {

  def ask[R] : Reader[R, R] = Reader(identity)

  def pure[R, A](a: A): Reader[R, A] = Reader(_ => a)

}
