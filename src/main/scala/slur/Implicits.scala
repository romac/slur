package slur

import scalaz._
import Scalaz._

object Implicits {

  implicit class ListValidationSequence[E, V](xs: List[Validation[E, V]]) {
    def sequenceV: Validation[E, List[V]] = {
      xs.foldLeft(List[V]().success[E]) {
        case (Failure(e), _) => e.failure
        case (Success(acc), Success(v)) => (acc :+ v).success[E]
        case (Success(acc), Failure(v)) => v.failure
      }
    }
  } 
  
}