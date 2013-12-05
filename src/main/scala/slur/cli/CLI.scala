package slur.cli

import slur._
import slur.ast._
import slur.runtime._
import java.io.FileReader
import java.util.Scanner
import scalaz._
import slur.parser.Parser

object CLI {

  val runtime = new Runtime
  val env = runtime.defaultEnv

  def main(args: Array[String]): Unit = args match {
    case Array() => runREPL()
    case Array(fileName, _*) => runFile(fileName)
  }

  def runREPL() = {
    val scanner = new Scanner(System.in)
    val repl = new REPL(scanner, runtime, env)

    repl.run()
  }

  def runFile(fileName: String) = {
    val fileReader = new FileReader(fileName)
    val result = Parser.parse(fileReader).flatMap(runtime.eval(env)(_))

    display(result)
  }

  def display(result: Validation[SlurError, SExpr]) = result match {
    case Success(value) => success(value)
    case Failure(err) => error(err)
  }

  def success(value: SExpr) = {
    println(value)
  }

  def error(msg: String) = {
    println(msg)
    System.exit(1)
  }

  def error(e: SlurError) = {
    println(e.msg)
    e.printStackTrace();
    System.exit(1)
  }

}
