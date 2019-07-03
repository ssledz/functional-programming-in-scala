package pl.softech.learning.ch3


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  object Implicits extends Ex25.Implicits with Ex26.Implicits with Ex27.Implicits
    with Ex28.Implicits with Ex29.Implicits

}