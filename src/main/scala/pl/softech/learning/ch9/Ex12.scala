package pl.softech.learning.ch9

import scala.util.matching.Regex


object Ex12 {

  class MyParser[+A]

  // TODO Implements MyParser & MyParsers
  object MyParsers extends Parsers[MyParser] {

    def run[A](p: MyParser[A])(input: String): Either[Parsers.ParseError, A] = ???

    def or[A](s1: MyParser[A], s2: => MyParser[A]): MyParser[A] = ???

    implicit def string(s: String): MyParser[String] = ???

    def slice[A](p: MyParser[A]): MyParser[String] = ???

    def many1[A](p: MyParser[A]): MyParser[List[A]] = ???

    def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = ???

    implicit def regex(r: Regex): MyParser[String] = ???

    def scope[A](msg: String)(p: MyParser[A]): MyParser[A] = ???

    def attempt[A](p: MyParser[A]): MyParser[A] = ???

  }

}
