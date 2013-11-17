import slur._

import scalaz._
import Scalaz._

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(157); 
 
  val v = List(Success(SNumber(1)), Failure(TypeError("fail1")), Failure(TypeError("fail2")));System.out.println("""v  : List[Product with Serializable with scalaz.Validation[slur.TypeError,slur.SNumber]] = """ + $show(v ))}
  
  
}
