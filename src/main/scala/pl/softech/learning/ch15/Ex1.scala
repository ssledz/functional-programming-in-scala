package pl.softech.learning.ch15

object Ex1 {

  trait ProcessOps {

    def take[I](n: Int): Process[I, I] = ???

    def drop[I](n: Int): Process[I, I] = ???

    def takeWhile[I](f: I => Boolean): Process[I, I] = ???

    def dropWhile[I](f: I => Boolean): Process[I, I] = ???

  }

  def main(args: Array[String]): Unit = {

  }

}
