package slur

import scalaz._
import Scalaz._
import Implicits._

abstract class SlurError extends Exception {
  def msg: String
  override def toString = s"Error: $msg"
}

abstract class RuntimeError extends SlurError

case class TypeMismatch(expected: String, found: SExpr) extends RuntimeError {
  def msg = s"Invalid type: expected '$expected', found '$found'. Stack: "
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
case class NotFunction(func: SExpr) extends RuntimeError {
  def msg = s"'$func' is not a function."
}
case class UnboundVariable(symbol: SSymbol) extends RuntimeError {
  def msg = s"Variable '$symbol' is unbound."
}
case class BadLambdaDef(expr: SExpr) extends RuntimeError {
  def msg = s"$expr is not a valid lambda definition."
}

class Runtime extends Builtins {
  
  def eval(env: Env)(expr: SExpr): Validation[RuntimeError, SExpr] = {
    // println(s"Evaluating '$expr' with environment:\n$env")
    expr match {
      case SList((func @ SSymbol(_)) :: args) => call(func, args.toList, env)
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
  }
  
  def call(funcExpr: SSymbol, args: List[SExpr], env: Env): Validation[RuntimeError, SExpr] = {
    // println("Calling " + funcExpr.toString + " with " + args.map(_.toString))
    eval(env)(funcExpr).flatMap {
      case f @ SNativeFunction(_, _) => f(args, env)
      case f @ SLambda(params, vararg, body, closure) => {
        if (params.length != args.length && vararg == None) {
          WrongArgumentNumber(f.toString, params.length, args.length).failure 
        }
        else {
          val func = new StdFunction(funcExpr.toString) {
            def applyEvaled(args: List[SExpr], env: Env): Validation[RuntimeError, SExpr] = {
              val remainingArgs = args.drop(params.length)
              closure ++= params.zip(args)
              vararg match {
                case Some(varargName) =>
                  closure += (varargName -> SList(remainingArgs))
                case None =>
              }
              // println(funcExpr.toString + ":\n" + closure.toString)
              body.map(eval(closure)).last
            }
          }
          func(args, env)
        }
      } 
      case e => NotFunction(e).failure
    }
  }
  
}