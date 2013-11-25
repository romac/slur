package slur

import scala.collection.immutable.{LinearSeq, StringLike}
import scala.collection.mutable.Builder
import scalaz._
import Scalaz._

trait Unpacker[T] {
  def unpack(e: SExpr): Validation[TypeError, T]
}

sealed trait SExpr {
  val typeName: String
}

case class SList(elements: List[SExpr]) extends SExpr with LinearSeq[SExpr] {
  val typeName = "List"
  
  def length = elements.length
  def apply(idx: Int): SExpr = elements(idx)
  override def iterator = elements.iterator
  override def toString =  "(" + elements.mkString(" ") + ")"
}

object SList {
  def apply(elements: SExpr*) = new SList(elements.toList)
}

case class SDottedList(elements: List[SExpr], override val last: SExpr) extends SExpr with LinearSeq[SExpr] {
  val typeName = "DottedList"
  
  def length = elements.length + 1
  def apply(idx: Int): SExpr = (elements :+ last).apply(idx)
  override def iterator = (elements :+ last).iterator
  override def toString =  "(" + elements.mkString(" ") + " . " + last + ")"
}

case class SNativeFunction(name: String, f: List[SExpr] => Validation[RuntimeError, SExpr]) extends SExpr {
  val typeName = "Native Function"
  
  def apply(args: List[SExpr]) = f(args)
  override def toString = s"<native function '$name'>" 
}

case class SLambda(params: List[String], vararg: Option[String], body: List[SExpr], closure: Env) extends SExpr {
  val typeName = "Lambda"
  
  override def toString =
    "(lambda (" ++ params.mkString(" ") ++ (vararg match {
      case Some(arg) => " . " ++ arg
      case None => ""
    }) ++ ") ...)"
}

case class SSymbol(name: String)    extends SExpr {
  val typeName = "Symbol"
  
  override def toString = name
}

object SSymbol extends Unpacker[String] {
  def unpack(e: SExpr): Validation[TypeError, String] = e match {
    case SSymbol(value) => value.success
    case _ => TypeError(s"Cannot unpack '$e' to a symbol.").failure
  }
}

case class SNumber(value: Double)   extends SExpr {
  val typeName = "Number"
    
  override def toString = value.toString
}

object SNumber extends Unpacker[Double] {
  def unpack(e: SExpr): Validation[TypeError, Double] = e match {
    case SNumber(value) => value.success
    case _ => TypeError(s"Cannot unpack '$e' to a number.").failure
  }
}

case class SBoolean(value: Boolean) extends SExpr {
  val typeName = "Boolean"
    
  override def toString = if (value) "#t" else "#f"
}

object SBoolean extends Unpacker[Boolean] {
  def unpack(e: SExpr): Validation[TypeError, Boolean] = e match {
    case SBoolean(value) => value.success
    case _ => TypeError(s"Cannot unpack '$e' to a boolean.").failure
  }
}

case class SString(value: String)   extends SExpr with StringLike[SString] {
  val typeName = "String"
    
  def seq = value.seq
  def newBuilder: Builder[Char, SString] = new StringBuilder mapResult(SString.apply)
  override def toString = "\"" + value + "\""
}

object SString extends Unpacker[String] {
  def unpack(e: SExpr): Validation[TypeError, String] = e match {
    case SString(value) => value.success
    case _ => TypeError(s"Cannot unpack '$e' to a string.").failure
  }
}

object SExpr {
  
  implicit def stringToSSymbol(s: String): SSymbol = SSymbol(s)
  implicit def booleanToSBoolean(b: Boolean): SBoolean = SBoolean(b)
  implicit def doubleToSNumber(n: Double): SNumber = SNumber(n)
  implicit def listToSList(xs: List[SExpr]): SList = SList(xs: _*)
  
}