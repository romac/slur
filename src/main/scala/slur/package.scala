package object slur {

  abstract class SlurError extends Exception {
    def msg: String
    override def toString = s"Error: $msg"
  }
  
}