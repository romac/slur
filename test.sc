package slur

import scalaz._
import Scalaz._
import Implicits._

object test {
 
  val a = new Env                                 //> a  : slur.Env = 
  a += ("a" -> SNumber(2))                        //> res0: scala.collection.mutable.HashMap[String,slur.SExpr] = Map(a -> 2.0)
  
  val b = new Env(Some(a))                        //> b  : slur.Env = a = 2.0
                                                  //| 
  b += ("b" -> SNumber(10))                       //> res1: scala.collection.mutable.HashMap[String,slur.SExpr] = Map(b -> 10.0)
   
  "\n" + a.toString                               //> res2: String = "
                                                  //| a = 2.0"
  "\n" + b.toString                               //> res3: String = "
                                                  //| a = 2.0
                                                  //|   b = 10.0"
   b("b")                                         //> res4: Option[slur.SExpr] = Some(10.0)
  
}