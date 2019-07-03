package pl.softech.learning.extra

object TreeSubsequence {

  trait Tree[+A]

  case class Leaf[+A](value: A) extends Tree[A]

  case class Branch[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

  case object Empty extends Tree[Nothing]

  def startsWith[A](t: Tree[A], prefix: Tree[A]): Boolean = (t, prefix) match {

    case (Leaf(x), Leaf(y)) => x == y
    case (Branch(x, xl, xr), Branch(y, yl, yr)) =>
      if (x != y) false
      else startsWith(xl, yl) && startsWith(xr, yr)
    case (Branch(x, xl, xr), Leaf(y)) => x == y
    case (Empty, Empty) => true
    case _ => false

  }

  def tails[A](t: Tree[A]): List[Tree[A]] = t match {
    case Empty => Nil
    case Leaf(_) => List(t)
    case Branch(v, l, r) => t :: tails(l) ::: tails(r)
  }

  def hasSubsequence[A](t: Tree[A], sub: Tree[A]): Boolean =
    tails(t).exists(startsWith(_, sub))

}

object TreeSubsequenceApp extends App {

  import TreeSubsequence._

  /**
                     1
                    /
                   2
                  / \
                 3  6
                / \
               4  5
 **/
  val t = Branch(1, Branch(2, Branch(3, Leaf(4), Leaf(5)), Leaf(6)), Empty)

  val p1 = Branch(3, Leaf(4), Leaf(5))
  val p2 = Branch(2, Leaf(3), Leaf(6))

  println(startsWith(p1, p1))
  println(startsWith(Leaf(1), p1))

  println(tails(p1))

  println(hasSubsequence(t, p1))
  println(hasSubsequence(t, p2))

}