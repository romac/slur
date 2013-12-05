package slur

import scala.collection.mutable.HashMap

class Env(val parent: Option[Env] = None) {

  val bindings: HashMap[String, SExpr] = HashMap()

  def +=(kv: (String, SExpr)) = bind(kv._1, kv._2)
  def ++=(newBindings: TraversableOnce[(String, SExpr)]) =
    bindings ++= newBindings

  def bind(name: String, value: SExpr) =
    bindings += (name -> value)

  def -=(name: String) = unbind(name)

  def unbind(name: String) =
    bindings -= name

  def lookupParent(name: String): Option[SExpr] =
    parent.flatMap(_.lookup(name))

  def apply(name: String) = lookup(name)

  def lookup(name: String): Option[SExpr] =
    bindings.get(name).orElse(lookupParent(name))

  def extend = new Env(Some(this))

  /*override def toString = asString(List.fill(parentsNum)("  ").mkString)

  protected def asString(indent: String): String = {
    val str = bindings.map(b => b._1 + " = " + b._2).map(indent + _).mkString("\n")
    val parentStr = parent match {
      case Some(env) => env.asString(indent.substring(2)) + "\n"
      case None => ""
    }

    parentStr + str
  }

  protected def parentsNum: Int = {
    def loop(n: Int) = parent match {
      case Some(p) => p.parentsNum + 1
      case None => 0
    }
    loop(0)
  }*/

}
