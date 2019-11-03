package pl.softech.learning.ch11

case class Id[A](value: A) extends AnyVal {

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

  def map[B](f: A => B): Id[B] = Id(f(value))

}

object Id {
  def pure[A](a: A): Id[A] = Id(a)
}
