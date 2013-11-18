package slur

import java.io.FileReader
import java.util.Scanner
import scalaz._

object CLI {

  val runtime = new Runtime
  
  def main(args: Array[String]): Unit = args match {
    case Array() => runREPL()
    case Array(fileName, _*) => runFile(fileName) 
  }
  
  def runREPL() = {
    val scanner = new Scanner(System.in)
    val repl = new REPL(scanner, runtime)
    
    repl.run()
  }
  
  def runFile(fileName: String) = {
    val fileReader = new FileReader(fileName)
    val result = Parser.parse(fileReader).flatMap(runtime.eval(_))
    
    display(result)
  }
  
  def display(result: Validation[SlurError, SExpr]) = result match {
    case Success(value) => success(value)
    case Failure(err) => error(err.msg)
  }
  
  def success(value: SExpr) = {
    println(value)
  }
  
  def error(msg: String) = {
    println(msg)
    System.exit(1)
  }
  
}