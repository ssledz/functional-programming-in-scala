package pl.softech.learning.ch9

import pl.softech.learning.ch9.Parsers.ParseError

import scala.util.matching.Regex

trait Parsers[Parser[+_]] extends Ex1.ParsersExt[Parser]
  with Ex3.ParsersExt[Parser] with Ex4.ParsersExt[Parser]
  with Ex7.ParsersExt[Parser] with Ex8.ParsersExt[Parser] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    // map(p)(a => a) == p
    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

  }

}

object Parsers {

  case class ParseError(stack: List[(Location, String)])

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }
  }

}