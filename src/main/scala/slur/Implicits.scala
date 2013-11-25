package slur

import scalaz._
import Scalaz._
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import java.util.concurrent.atomic.AtomicBoolean

object Implicits {

  /**
   * Implicitly add a `sequenceV` method to `List[Validation[E, V]]`.
   *
   * @fixme Use scalaz's `sequenceU` instead. It doesn't work out of the box
   *        but I suspect it is because `E` (or actually `RuntimeError`) is not
   *        a semi-group (which is needed in order to be able to accumulate errors within Validation).
   */
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
