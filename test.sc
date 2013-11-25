package slur

import scalaz._
import Scalaz._
import Implicits._

object test {
 
 val tests = Seq(
   """('a 2 "test")""",
   """(a 1.0 "test")""",
   """(a world "test")""",
   """(+ world "test")""",
   """(+ 1 2 . 3)""",
   "\n(a b c d)\n(1 2 3)\n"
 )                                                //> tests  : Seq[String] = List(('a 2 "test"), (a 1.0 "test"), (a world "test"),
                                                  //|  (+ world "test"), (+ 1 2 . 3), "
                                                  //| (a b c d)
                                                  //| (1 2 3)
                                                  //| ")
 val results = tests.map(Parser.parse)            //> results  : Seq[scalaz.Validation[slur.ParseError,slur.SExpr]] = List(Success
                                                  //| (((quote a) 2.0 "test")), Success((a 1.0 "test")), Success((a world "test"))
                                                  //| , Success((+ world "test")), Success((+ 1.0 2.0 . 3.0)), Failure(Parse error
                                                  //| : `(' expected but `
                                                  //| ' found))
 val ok = results.map {
   case Success(e) => e.toString
   case Failure(f) => f
 }                                                //> ok  : Seq[java.io.Serializable] = List(((quote a) 2.0 "test"), (a 1.0 "test"
                                                  //| ), (a world "test"), (+ world "test"), (+ 1.0 2.0 . 3.0), Parse error: `(' e
                                                  //| xpected but `
                                                  //| ' found)
 "\n" + ok.mkString("\n")                         //> res0: String = "
                                                  //| ((quote a) 2.0 "test")
                                                  //| (a 1.0 "test")
                                                  //| (a world "test")
                                                  //| (+ world "test")
                                                  //| (+ 1.0 2.0 . 3.0)
                                                  //| Parse error: `(' expected but `
                                                  //| ' found"
 
  
}