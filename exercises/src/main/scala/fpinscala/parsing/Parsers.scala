package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex

// so inner classes may call methods of trait
trait Parsers[ParseError, Parser[+_]] { self =>
  private def listCons[A](a: A, as: List[A]): List[A] = a :: as

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser
    [A]
    (a: A)
    (implicit f: A => Parser[String]):
    ParserOps[String] =
    ParserOps(f(a))

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  /*
  string("") will always succeed because it needs to 'parse' an empty
  string out of any given input string, empty or not.
  */
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(listCons) or succeed(List.empty)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(listCons)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    for {
      a <- pa
      b <- pb
    } yield (a, b)

  def map2
    [A, B, C]
    (pa: Parser[A], pb: => Parser[B])
    (f: (A, B) => C):
    Parser[C] =
    product(pa, pb).map { case (a, b) => f(a, b) }

  def map2ViaFlatMap
  [A, B, C]
  (pa: Parser[A], pb: => Parser[B])
    (f: (A, B) => C):
  Parser[C] =
    for {
      a <- pa
      b <- pb
    } yield f(a, b)

  def slice[A](p: Parser[A]): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n < 1) succeed(List.empty)
    else map2(p, listOfN(n - 1, p))(listCons)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /**
  Parses an integer followed by the same number of 'a' characters as the
  parsed integer.

  Returns the count of the parsed 'a' characters.
  */
  def listOfNAs: Parser[Int] =
    for {
      numStr <- "[1-9][0-9]*".r
      as <- listOfN(numStr.toInt, string("a"))
    } yield as.length

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]) = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]) = self.or(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  private def unbiasRight[A, B, C](triple: (A, (B, C))): (A, B, C) =
    (triple._1, triple._2._1, triple._2._2)

  private def unbiasLeft[A, B, C](triple: ((A, B), C)): (A, B, C) =
    (triple._1._1, triple._1._2, triple._2)

  object Laws {
    import org.scalacheck.{ Gen, Prop }
    import Prop._

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)

    val succeedLaw: Prop =
      forAll { (a: String, in: String) =>
        run(succeed(a))(in) == Right(a)
      }

    def productLaw
    [A, B, C]
    (pa: Parser[A], pb: Parser[B], pc: Parser[C])
    (in: Gen[String]):
    Prop =
      equal(
        ((pa ** pb) ** pc).map(unbiasLeft),
        (pa ** (pb ** pc)).map(unbiasRight)
      )(in)
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
