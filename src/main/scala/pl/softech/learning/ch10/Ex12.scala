package pl.softech.learning.ch10

import pl.softech.learning.ch10.FoldableInstances._

object Ex12 {

  trait FoldableInstances {

    implicit val foldableListInstance: Foldable[List] = new Foldable[List] {

      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero) { (acc, a) =>
        mb.op(acc, f(a))
      }
    }

    implicit val foldableIndexedSeqInstance: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
      override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero) { (acc, a) =>
        mb.op(acc, f(a))
      }
    }

    implicit val foldableStreamInstance: Foldable[Stream] = new Foldable[Stream] {
      override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

      override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

      override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero) { (acc, a) =>
        mb.op(acc, f(a))
      }
    }

  }

  def main(args: Array[String]): Unit = {

    println(Foldable[List].foldLeft(List(1, 2, 3, 4, 5))(0)(_ + _))
    println(Foldable[Stream].foldLeft(Stream(1, 2, 3, 4, 5))(0)(_ + _))
    println(Foldable[IndexedSeq].foldLeft(IndexedSeq(1, 2, 3, 4, 5))(0)(_ + _))

  }


}
