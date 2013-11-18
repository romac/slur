package slur

import scalaz._
import Scalaz._

abstract class SlurError {
  def msg: String
  override def toString = s"Error: $msg"
}

abstract class RuntimeError extends SlurError

case class TypeMismatch(expected: String, found: SExpr) extends RuntimeError {
  def msg = s"Invalid type: expected '$expected', found '$found'."
}

case class TypeError(_msg: String) extends RuntimeError {
  def msg = s"Type error: ${_msg}."
}

case class BadSpecialForm(expr: SExpr) extends RuntimeError {
  def msg = s"Bad special form: Unrecognized special form SExpr."
}

case class WrongArgumentNumber(func: String, expected: Int, found: Int) extends RuntimeError {
  def msg = s"Wrong argument number: Function '$func' expects $expected arguments, found $found."   
}
case class FunctionNotFound(func: String) extends RuntimeError {
  def msg = s"Function '$func' not found." 
}
case class NotFunction(func: SExpr) extends RuntimeError {
  def msg = s"'$func' is not a function."
}
case class UnboundVariable(symbol: SSymbol) extends RuntimeError {
  def msg = s"Variable '$symbol' is unbound."
}

class Runtime(val env: Env) extends Builtins {
  
  def eval(expr: SExpr): Validation[RuntimeError, SExpr] = expr match {
    case SList(SSymbol(func) :: args) => call(func, args.toList)
    case SNumber(_) => expr.success
    case SString(_) => expr.success
    case SBoolean(_) => expr.success
    case SList(func :: _) => NotFunction(func).failure
    case v @ SSymbol(name) => env(name) match {
      case Some(value) => value.success
      case None => UnboundVariable(v).failure
    }
    case _ => BadSpecialForm(expr).failure
  }
  
  def call(funcName: String, args: List[SExpr]): Validation[RuntimeError, SExpr] = {
    val func = builtins.getOrElse(funcName, funcNotFound(funcName) _)
    func(args)
  }
  
  def funcNotFound(name: String)(args: List[SExpr]): Validation[RuntimeError, SExpr] =
    FunctionNotFound(name).failure
  
}