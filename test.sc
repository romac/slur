package slur

import scalaz._
import Scalaz._
import slur.errors.RuntimeErrors._
// import slur.util.Implicits._

object test {

  /*implicit val errorSemigroup = new Semigroup[RuntimeError] {
    def append(f1: RuntimeError, f2: => RuntimeError): RuntimeError =
      new RuntimeError {
        def msg = f1.msg + "\n" + f2.msg
      }
  }*/

  def success1 = 1.successNelNel[RuntimeError]       //> success1: => scalaz.ValidationNel[[slur.errors.RuntimeErrors.RuntimeError,Int
                                                  //| ]
  def success2 = 2.successNelNel[RuntimeError]       //> success2: => scalaz.ValidationNel[[slur.errors.RuntimeErrors.RuntimeError,Int
                                                  //| ]
  def failure1 = TypeError("type error").failureNelNel[Int]
                                                  //> failure1: => scalaz.ValidationNel[[slur.errors.RuntimeErrors.TypeError,Int]
  def failure2 = TypeError("type mismatch").failureNelNel[Int]
                                                  //> failure2: => scalaz.ValidationNel[[slur.errors.RuntimeErrors.TypeError,Int]

  val res = (success1 |@| failure1 |@| failure2) { _ + _ + _ }
                                                  //> res  : scalaz.Unapply[scalaz.Apply,scalaz.ValidationNel[[slur.errors.RuntimeE
                                                  //| rrors.RuntimeError,Int]]{type M[X] = scalaz.ValidationNel[[slur.errors.Runtim
                                                  //| eErrors.RuntimeError,X]; type A = Int}#M[Int] = Failure(NonEmptyList(Error:
                                                  //| Type error: type error., Error: Type error: type mismatch.))

  val l = List(success1, failure1, success2, failure2)
                                                  //> l  : List[scalaz.ValidationNel[[slur.errors.RuntimeErrors.RuntimeError,Int]]
                                                  //| = List(Success(1), Failure(NonEmptyList(Error: Type error: type error.)), Su
                                                  //| ccess(2), Failure(NonEmptyList(Error: Type error: type mismatch.)))

  l.sequenceU                                     //> res0: scalaz.ValidationNel[scalaz.NonEmptyList[slur.errors.RuntimeErrors.Runtim
                                                  //| eError],List[Int]] = Failure(NonEmptyList(Error: Type error: type error., Er
                                                  //| ror: Type error: type mismatch.))



}
