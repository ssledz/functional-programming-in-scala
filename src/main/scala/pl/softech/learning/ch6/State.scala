package pl.softech.learning.ch6

case class State[S, +A](run: S => (A, S)) {
  self =>

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (x, ss) = run(s)
    val newState = f(x)
    newState.run(ss)
  })

  def map[B](f: A => B): State[S, B] = flatMap(x => State(s => (f(x), s)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- self
    b <- sb
  } yield f(a, b)

}

object State {

  def unit[S, A](a: A): State[S, A] = State((a, _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] = State(s =>
    xs match {
      case h :: t => {
        val ret = for {
          y <- h
          ys <- sequence(t)
        } yield y :: ys
        ret.run(s)
      }
      case Nil => (List.empty, s)
    }
  )

}
