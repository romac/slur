package slur

import scalaz._
import Scalaz._
import Implicits._

object test {
 
  val v: List[Validation[RuntimeError, SExpr]] = List(Success(SNumber(1)), Failure(TypeError("fail1")), Failure(TypeError("fail2")))
                                                  //> v  : List[scalaz.Validation[slur.RuntimeError,slur.SExpr]] = List(Success(1.
                                                  //| 0), Failure(Error: Type error: fail1.), Failure(Error: Type error: fail2.))
  v.sequenceV                                     //> res0: scalaz.Validation[slur.RuntimeError,List[slur.SExpr]] = Failure(Error:
                                                  //|  Type error: fail1.)
  
}