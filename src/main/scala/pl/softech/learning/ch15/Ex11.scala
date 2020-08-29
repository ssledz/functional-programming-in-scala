package pl.softech.learning.ch15

object Ex11 {

  trait ProcessOps {

    def eval[F[_], A](fa: F[A]): Process[F, A] = ???

    def eval_[F[_], A, B](fa: F[A]): Process[F, B] = ???

  }

  def main(args: Array[String]): Unit = {

  }

}
