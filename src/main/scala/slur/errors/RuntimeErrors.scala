package slur.errors

import slur.SlurError
import slur.ast._

object RuntimeErrors {
  
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
  
}
