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
  val typeName = "list"
  
  def length = elements.length
  def apply(idx: Int) = elements(idx)
  override def iterator = elements.iterator
  override def toString =  "(" + elements.mkString(" ") + ")"
}

object SList {
  def apply(elements: SExpr*) = new SList(elements.toList)
}

case class SSymbol(name: String)    extends SExpr {
  val typeName = "symbol"
    
  override def toString = name
}

object SSymbol extends Unpacker[String] {
  def unpack(e: SExpr): Validation[TypeError, String] = e match {
    case SSymbol(value) => value.success
    case _ => TypeError(s"Cannot unpack '$e' to a symbol.").failure
  }
}

case class SNumber(value: Double)   extends SExpr {
  val typeName = "number"
    
  override def toString = value.toString
}

object SNumber extends Unpacker[Double] {
  def unpack(e: SExpr): Validation[TypeError, Double] = e match {
    case SNumber(value) => value.success
    case _ => TypeError(s"Cannot unpack '$e' to a number.").failure
  }
}

case class SBoolean(value: Boolean) extends SExpr {
  val typeName = "boolean"
    
  override def toString = if (value) "#t" else "#f"
}

object SBoolean extends Unpacker[Boolean] {
  def unpack(e: SExpr): Validation[TypeError, Boolean] = e match {
    case SBoolean(value) => value.success
    case _ => TypeError(s"Cannot unpack '$e' to a boolean.").failure
  }
}

case class SString(value: String)   extends SExpr with StringLike[SString] {
  val typeName = "string"
    
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