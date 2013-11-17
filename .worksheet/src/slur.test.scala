package slur

import scalaz._
import Scalaz._
import Implicits._

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(214); 
 
  val v: List[Validation[RuntimeError, SExpr]] = List(Success(SNumber(1)), Failure(TypeError("fail1")), Failure(TypeError("fail2")));System.out.println("""v  : List[scalaz.Validation[slur.RuntimeError,slur.SExpr]] = """ + $show(v ));$skip(14); val res$0 = 
  v.sequenceV;System.out.println("""res0: scalaz.Validation[slur.RuntimeError,List[slur.SExpr]] = """ + $show(res$0))}
  
}
