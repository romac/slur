package slur

import scala.collection.mutable.HashMap

class Env(parent: Option[Env] = None) {
  
  val bindings: HashMap[String, SExpr] = HashMap()
  
  def +=(kv: (String, SExpr)) = bind(kv._1, kv._2)
  
  def bind(name: String, value: SExpr) =
    bindings += (name -> value)
  
  def +=(name: String) = unbind(name)
  
  def unbind(name: String) =
    bindings -= name
  
  def lookupParent(name: String): Option[SExpr] =
    parent.flatMap(_.lookup(name))
  
  def apply(name: String) = lookup(name)
    
  def lookup(name: String): Option[SExpr] =
    bindings.get(name).orElse(lookupParent(name))
  
  def extend = new Env(Some(this))
  
  def close = parent
    
}