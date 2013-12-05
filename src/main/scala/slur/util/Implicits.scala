package slur.util

import scalaz._
import scalaz.Scalaz._

object Implicits {

  /**
   * Not needed anymore, since we're using ValidationNel instead of plain Validation.
   * 
   * Implicitly add a `sequenceV` method to `List[Validation[E, V]]`.
   *
   * @fixme Use scalaz's `sequenceU` instead. It doesn't work out of the box
   *        but I suspect it is because `E` (or actually `RuntimeError`) is not
   *        a scalaz SemiGroup (which is needed in order to be able to accumulate errors within Validation).
   */
  /*implicit class ListValidationSequence[E, V](xs: List[Validation[E, V]]) {
    def sequenceV: Validation[E, List[V]] = {
      xs.foldLeft(List[V].empty.success[E]) {
        case (Failure(e), _) => e.failure
        case (Success(acc), Success(v)) => (acc :+ v).success[E]
        case (Success(acc), Failure(v)) => v.failure
      }
    }
  }*/

}
