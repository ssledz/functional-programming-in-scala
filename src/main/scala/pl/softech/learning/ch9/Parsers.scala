package pl.softech.learning.ch9

import pl.softech.learning.Assertion._

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] extends Ex1.ParsersExt[ParseError, Parser]
  with Ex3.ParsersExt[ParseError, Parser] with Ex4.ParsersExt[ParseError, Parser]
  with Ex7.ParsersExt[ParseError, Parser] with Ex8.ParsersExt[ParseError, Parser] {
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

  object Laws {

    def laws(): Unit = {

      for (c <- 'A' to 'z') {
        run(char(c))(c.toString) === Right(c)

        List("alaa").foreach { a =>
          run(succeed(a))(c.toString) == Right(a)
        }

      }

      List("ababa").foreach { s =>
        run(string(s))(s) === Right(s)

        List("qwerty").foreach { a =>
          run(succeed(a))(s) == Right(a)
        }

      }

      run(or(string("abra"), string("cadabra")))("abra") === Right("abra")

      run(or(string("abra"), string("cadabra")))("cadabra") === Right("cadabra")

      run(listOfN(3, "ab" | "cad"))("ababcad") === Right(List("ab", "ab", "cad"))

      run(listOfN(3, "ab" | "cad"))("cadabab") === Right(List("cad", "ab", "ab"))

      run(listOfN(3, "ab" | "cad"))("ababab") === Right(List("ab", "ab", "ab"))

      val numA: Parser[Int] = char('a').many.map(_.size)

      run(numA)("aaa") === Right(3)

      run(slice((char('a') | char('b')).many))("aaba") === Right("aaba")

    }

  }

}
