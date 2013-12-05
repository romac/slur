package slur.cli

import slur.ast._
import slur.runtime._
import scalaz._
import scalaz.Scalaz._
import java.util.Scanner
import slur.parser.Parser

class REPL(scanner: Scanner = new Scanner(System.in), runtime: Runtime = new Runtime, env: Env = new Env) {

  sealed trait Input
  case class Expression(expr: String) extends Input
  case class Command(cmd: String) extends Input

  sealed trait Output
  case class Value(expr: SExpr) extends Output {
    override def toString = expr.toString
  }
  case class Info(msg: String) extends Output {
  override def toString = msg
  }
  case class Error(msg: String) extends Output {
    override def toString = "Error: " + msg
  }
  case object None extends Output {
    override def toString = ""
  }

  val commands = Map(
      "quit" -> quit _,
      "help" -> help _
  )

  def run(): Unit = {
    println("Slur 0.1, Read-Eval-Print-Loop")
    repl()
  }

  def quit(): Output = {
    System.exit(0)
    None
  }

  def help(): Output = Info("""Commands:
  - quit: Quit the REPL
  - help: Display this help""")

  def repl(): Output = {
    val input = read
    val output = eval(input)
    println(output)
    repl()
  }

  def read: Input = {
    print("> ")

    val input = scanner.nextLine()

    if (input.startsWith(":"))
      Command(input.substring(1))
    else
      Expression(input)
  }

  def eval(input: Input): Output = input match {
    case Expression(raw) => Parser.parse(raw).flatMap(runtime.eval(env)(_)) match {
      case Success(expr) => Value(expr)
      case Failure(err) => Error(err.msg)
    }
    case Command(cmd) => exec(cmd)
  }

  def exec(cmd: String): Output =
    if (commands isDefinedAt cmd)
      commands(cmd).apply
    else
      Error(s"Command '$cmd' not found.")

}
