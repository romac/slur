
package slur.ast

import slur._
import slur.runtime._
import slur.errors.RuntimeErrors._
import scala.collection.immutable.{LinearSeq, StringLike}
import scala.collection.mutable.Builder
import scalaz._
import scalaz.Scalaz._

trait Unpacker[T] {
  def unpack(e: SExpr): ValidationNel[TypeError, T]
}

sealed trait SExpr {
  def typeName = this.getClass.getName
}

case class SList(elements: List[SExpr]) extends SExpr with LinearSeq[SExpr] {
  def length = elements.length
  def apply(idx: Int): SExpr = elements(idx)
  override def iterator = elements.iterator
  override def toString =  "(" + elements.mkString(" ") + ")"
}

object SList {
  def apply(elements: SExpr*) = new SList(elements.toList)
}

case class SDottedList(elements: List[SExpr], override val last: SExpr) extends SExpr with LinearSeq[SExpr] {
  def length = elements.length + 1
  def apply(idx: Int): SExpr = (elements :+ last).apply(idx)
  override def iterator = (elements :+ last).iterator
  override def toString =  "(" + elements.mkString(" ") + " . " + last + ")"
}

case class SNativeFunction(name: String, f: (List[SExpr], Env) => ValidationNel[RuntimeError, SExpr]) extends SExpr {
  override def toString = s"<native function '$name'>"
}

case class SLambda(params: List[String], vararg: Option[String], body: List[SExpr], closure: Env) extends SExpr {
  override def toString =
    "(lambda (" ++ params.mkString(" ") ++ (vararg match {
      case Some(arg) => " . " ++ arg
      case None => ""
    }) ++ ") ...)"
}

case class SSymbol(name: String)    extends SExpr {
  override def toString = name
}

object SSymbol extends Unpacker[String] {
  def unpack(e: SExpr): ValidationNel[TypeError, String] = e match {
    case SSymbol(value) => value.successNel
    case _ => TypeError(s"Cannot unpack '$e' to a symbol.").failureNel
  }
}

case class SNumber(value: Double)   extends SExpr {
  override def toString = value.toString
}

object SNumber extends Unpacker[Double] {
  def unpack(e: SExpr): ValidationNel[TypeError, Double] = e match {
    case SNumber(value) => value.successNel
    case _ => TypeError(s"Cannot unpack '$e' to a number.").failureNel
  }
}

case class SBoolean(value: Boolean) extends SExpr {
  override def toString = if (value) "#t" else "#f"
}

object SBoolean extends Unpacker[Boolean] {
  def unpack(e: SExpr): ValidationNel[TypeError, Boolean] = e match {
    case SBoolean(value) => value.successNel
    case _ => TypeError(s"Cannot unpack '$e' to a boolean.").failureNel
  }
}

case class SString(value: String)   extends SExpr with StringLike[SString] {
  def seq = value.seq
  def newBuilder: Builder[Char, SString] = new StringBuilder mapResult(SString.apply)
  override def toString = "\"" + value + "\""
}

object SString extends Unpacker[String] {
  def unpack(e: SExpr): ValidationNel[TypeError, String] = e match {
    case SString(value) => value.successNel
    case _ => TypeError(s"Cannot unpack '$e' to a string.").failureNel
  }
}

object SExpr {

  implicit def stringToSSymbol(s: String): SSymbol = SSymbol(s)
  implicit def booleanToSBoolean(b: Boolean): SBoolean = SBoolean(b)
  implicit def doubleToSNumber(n: Double): SNumber = SNumber(n)
  implicit def listToSList(xs: List[SExpr]): SList = SList(xs: _*)

}
