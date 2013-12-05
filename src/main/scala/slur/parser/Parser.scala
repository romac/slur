package slur.parser

import slur._
import slur.ast._
import scalaz._
import scalaz.Scalaz._
import scala.util.parsing.combinator._
import java.io.FileReader

case class ParseError(val msg: String) extends SlurError {
  override def toString = s"Parse error: $msg"
}

class SExprParsers extends JavaTokenParsers with PackratParsers {

  type P[T] = PackratParser[T]
  type PE = PackratParser[SExpr]

  override val whiteSpace = """""".r

  val space = """[ \t]+""".r 
  
  val eol = """(\r?\n)+""".r

  lazy val initial = letter | special

  lazy val letter = """[A-Za-z]""".r

  lazy val special = "!" | "$" | "%" | "&" | "+" | "-" | "*" | "/" | "<" | "=" | ">" | "?" | "^" | "_" | "~"

  lazy val subseqent = initial | digit

  lazy val digit = """[\d]""".r

  lazy val token: PE = identifier | boolean | number | string | quoted

  lazy val identifier: PE = (initial ~ rep(subseqent)) ^^ { case (x ~ xs) => SSymbol(x + xs.mkString) }

  lazy val boolean: PE = ("#t" | "#f") ^^ { b => SBoolean(b == "#t") }

  lazy val number: PE = floatingPointNumber ^^ { n => SNumber(n.toDouble) }

  lazy val string: PE = stringLiteral ^^ { s => SString(s.tail.init) }

  lazy val quoted: PE = ("'" ~> expr) ^^ { e => SList(SSymbol("quote"), e) }

  lazy val list: PE = repsep(expr, space) ^^ { xs => SList(xs: _*) }

  lazy val dottedList: PE = (repsep(expr, space) ~ (space ~> "." ~> space ~> expr)) ^^ { case (head ~ tail) => SDottedList(head, tail) }

  lazy val expr: PE = token | ("(" ~> (list ||| dottedList) <~ ")")

  lazy val program: P[List[SExpr]] = (space *) ~> phrase(expr *) <~ (space *)

}

object Parser extends SExprParsers {

  import java.io.FileReader

  def parse(in: String): Validation[ParseError, SExpr] = {
    parseAll(phrase(expr), in) match {
      case Success(e, _) => e.success
      case f: NoSuccess  => ParseError(f.msg).failure
    }
  }

  def parse(in: FileReader): Validation[ParseError, SExpr] = {
    parseAll(phrase(expr), in) match {
      case Success(e, _) => e.success
      case f: NoSuccess  => ParseError(f.msg).failure
    }
  }
}
