package slur

import scalaz._
import Scalaz._

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical.StdLexical

class ParseError(val msg: String) extends SlurError {
  override def toString = s"Parse error: $msg" 
}

class SExprParsers extends JavaTokenParsers {
  
  override val whiteSpace = "".r
  
  def space = """\s+""".r
  
  def token: Parser[SExpr] = identifier | boolean | number | string | quoted
  
  def identifier: Parser[SSymbol] = (initial ~ rep(subseqent)) ^^ { case (x ~ xs) => SSymbol(x + xs.mkString) }
  
  def initial = letter | special
  
  def letter = """[A-Za-z]""".r
  
  def special = "!" | "$" | "%" | "&" | "+" | "-" | "*" | "/" | "<" | "=" | ">" | "?" | "^" | "_" | "~"
  
  def subseqent = initial | digit 
  
  def digit = """[\d]""".r
  
  def boolean = ("#t" | "#f") ^^ { b => SBoolean(b == "#t") }
  
  def number = floatingPointNumber ^^ { n => SNumber(n.toDouble) }
  
  def string = stringLiteral ^^ { s => SString(s.tail.init) }
  
  def quoted = ("'" ~> expr) ^^ { e => SList(SSymbol("quote"), e) }
  
  def list: Parser[SList] = ("(" ~> repsep(expr, space) <~ ")") ^^ { xs => SList(xs: _*) } 
  
  def expr: Parser[SExpr] = token | list
  
}

object Parser extends SExprParsers {
  
  import java.io.FileReader
  
  def parse(in: String): Validation[ParseError, SExpr] = {
    parseAll(expr, in) match {
      case Success(e, _) => e.success
      case f: NoSuccess  => new ParseError(f.msg).failure
    }
  }
  
  def parse(in: FileReader): Validation[ParseError, SExpr] = {
    parseAll(expr, in) match {
      case Success(e, _) => e.success
      case f: NoSuccess  => new ParseError(f.msg).failure
    }
  }
}
