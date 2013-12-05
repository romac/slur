package slur.runtime

import slur.ast._
import slur.errors.RuntimeErrors._
import slur.util.Implicits._

import scalaz._
import scalaz.Scalaz._

class Runtime extends Builtins {

  def eval(env: Env)(expr: SExpr): ValidationNel[RuntimeError, SExpr] = {
    expr match {
      case SList((func @ SSymbol(_)) :: args) => call(func, args.toList, env)
      case SNumber(_) => expr.successNel
      case SString(_) => expr.successNel
      case SBoolean(_) => expr.successNel
      case SList(func :: _) => NotFunction(func).failureNel
      case v @ SSymbol(name) => env(name) match {
        case Some(value) => value.successNel
        case None => UnboundVariable(v).failureNel
      }
      case _ => BadSpecialForm(expr).failureNel
    }
  }

  def call(funcExpr: SSymbol, args: List[SExpr], env: Env): ValidationNel[RuntimeError, SExpr] = {
    eval(env)(funcExpr).flatMap {
      case SNativeFunction(name, f) => f(args, env)
      case f @ SLambda(params, vararg, body, closure) => {
        if (params.length != args.length && vararg == None) {
          WrongArgumentNumber(f.toString, params.length, args.length).failureNel
        }
        else {
          val func = new StdFunction(funcExpr.toString) {
            def applyEvaled(args: List[SExpr], env: Env): ValidationNel[RuntimeError, SExpr] = {
              val remainingArgs = args.drop(params.length)
              closure ++= params.zip(args)
              vararg match {
                case Some(varargName) =>
                  closure += (varargName -> SList(remainingArgs))
                case None =>
              }
              body.map(eval(closure)).last
            }
          }
          func(args, env)
        }
      }
      case e => NotFunction(e).failureNel
    }
  }

}
