package slur.runtime

// TODO: Use implicits instead of explicit pack/unpackers.

import slur.ast._
import slur.errors.RuntimeErrors._
import slur.util.Implicits._
import scalaz._
import scalaz.Scalaz._
import slur.ast.SExpr.listToSList
import slur.ast.SExpr.stringToSSymbol

trait Builtins { self: Runtime =>

  trait PackUnpack[A, B] { self: Function =>

    val packer: B => SExpr
    val unpacker: SExpr => ValidationNel[RuntimeError, A]

    protected def unpack(xs: List[SExpr]): ValidationNel[RuntimeError, List[A]] =
      xs.map(unpacker).sequenceU

    protected def unpack(e: SExpr): ValidationNel[RuntimeError, A] =
      unpacker(e)

    protected def pack(value: B): SExpr =
      packer(value)

    def unpackError = s"Some arguments of '$name' are of unexpected type."
  }

  abstract class Function(val name: String)
      extends ((List[SExpr], Env) => ValidationNel[RuntimeError, SExpr]) {

    def apply(args: List[SExpr], env: Env): ValidationNel[RuntimeError, SExpr]
  }

  abstract class StdFunction(name: String) extends Function(name) {

    def applyEvaled(args: List[SExpr], env: Env): ValidationNel[RuntimeError, SExpr]

    def apply(args: List[SExpr], env: Env): ValidationNel[RuntimeError, SExpr] = {
      for {
        evaled <- args.map(eval(env)).sequenceU
        result <- applyEvaled(evaled, env)
      } yield result
    }
  }

  abstract class UnaryFunction[A, B](name: String)
      extends StdFunction(name) with PackUnpack[A, B] {

    def op(a: A): B

    def applyEvaled(args: List[SExpr], env: Env): ValidationNel[RuntimeError, SExpr] = args match {
      case x :: Nil => for (a <- unpack(x)) yield pack(op(a))
      case _ => WrongArgumentNumber(name, 1, args.length).failureNel
    }

  }

  abstract class BinaryFunction[A, B](name: String)
      extends StdFunction(name) with PackUnpack[A, B] {

    def op(a: A, b: A): B

    def applyEvaled(args: List[SExpr], env: Env): ValidationNel[RuntimeError, SExpr] = args match {
      case x :: y :: Nil =>
        for {
          a <- unpack(x)
          b <- unpack(y)
        } yield pack(op(a, b))
      case _ => WrongArgumentNumber(name, 2, args.length).failureNel
    }

  }

  abstract class FoldFunction[T, Z](name: String)
      extends StdFunction(name) with PackUnpack[T, Z] {

    val zero: Z
    def fold(acc: Z, elem: T): Z

    def applyEvaled(args: List[SExpr], env: Env): ValidationNel[RuntimeError, SExpr] = args match {
      case Nil => pack(zero).successNel
      case x :: xs =>
        for (unpacked <- unpack(args))
          yield pack(unpacked.foldLeft(zero)(fold))
    }
  }

  abstract class ReduceFunction[A](name: String)
      extends StdFunction(name) with PackUnpack[A, A] {

    val zero: A
    def reduce(a: A, b: A): A

    def applyEvaled(args: List[SExpr], env: Env): ValidationNel[RuntimeError, SExpr] = args match {
      case Nil => pack(zero).successNel
      case x :: xs =>
        for (unpacked <- unpack(args))
          yield pack(unpacked.reduce(reduce))
    }
  }

  def numBinOp(name: String)(_op: (Double, Double) => Double): Function = {
    new BinaryFunction[Double, Double](name) {
      val packer = SNumber.apply _
      val unpacker = SNumber.unpack _
      def op(a: Double, b: Double) = _op(a, b)
    }
  }

  def boolBinOp(name: String)(_op: (Boolean, Boolean) => Boolean): Function = {
    new BinaryFunction[Boolean, Boolean](name) {
      val packer = SBoolean.apply _
      val unpacker = SBoolean.unpack _
      def op(a: Boolean, b: Boolean) = _op(a, b)
    }
  }

  def numBoolBinOp(name: String)(_op: (Double, Double) => Boolean): Function = {
    new BinaryFunction[Double, Boolean](name) {
      val packer = SBoolean.apply _
      val unpacker = SNumber.unpack _
      def op(a: Double, b: Double) = _op(a, b)
    }
  }

  val add = numBinOp("+")(_ + _)
  val diff = numBinOp("-")(_ - _)
  val mul = numBinOp("*")(_ * _)
  val div = numBinOp("/")(_ / _)

  val eq = numBoolBinOp("=")(_ == _)
  val neq = numBoolBinOp("/=")(_ != _)
  val gt = numBoolBinOp(">")(_ > _)
  val gte = numBoolBinOp(">=")(_ >= _)
  val lt = numBoolBinOp("<")(_ < _)
  val lte = numBoolBinOp("<=")(_ <= _)

  val and = boolBinOp("&&")(_ && _)
  val or = boolBinOp("||")(_ || _)

  object sum extends ReduceFunction[Double]("sum") {
    val zero = 0.0
    val packer = SNumber.apply _
    val unpacker = SNumber.unpack _
    def reduce(a: Double, b: Double) = a + b
  }

  object quote extends Function("quote") {
    override def apply(args: List[SExpr], env: Env) = args match {
      case x :: Nil => x.successNel
      case _ => WrongArgumentNumber(name, 1, args.length).failureNel
    }
  }

  object ifStmt extends Function("if") {
    override def apply(args: List[SExpr], env: Env) = args match {
      case pred :: conseq :: alt :: Nil => eval(env)(pred).flatMap {
        case SBoolean(true) => eval(env)(conseq)
        case SBoolean(false) => eval(env)(alt)
        case other => TypeMismatch("boolean", other.typeName).failureNel
      }
      case _ => WrongArgumentNumber(name, 3, args.length).failureNel
    }
  }

  object car extends StdFunction("car") {
    def applyEvaled(args: List[SExpr], env: Env) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(x :: xs) => x.successNel
        case SDottedList(x :: xs, _) => x.successNel
        case other => TypeMismatch("list", other.typeName).failureNel
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failureNel
    }
  }

  object cdr extends StdFunction("cdr") {
    def applyEvaled(args: List[SExpr], env: Env) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(x :: xs) => SList(xs).successNel
        case SDottedList(List(_), x) => x.successNel
        case SDottedList(_ :: xs, x) => SDottedList(xs, x).successNel
        case other => TypeMismatch("list", other.typeName).failureNel
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failureNel
    }
  }

  object cons extends StdFunction("cons") {
    def applyEvaled(args: List[SExpr], env: Env) = args match {
      case arg1 :: arg2 :: Nil => (arg1, arg2) match {
        case (x, SList(Nil)) => SList(x :: Nil).successNel
        case (x, SList(xs)) => SList(x :: xs).successNel
        case (x, SDottedList(xs, xlast)) => SDottedList(x :: xs, xlast).successNel
        case (x, y) => SList(x :: y :: Nil).successNel
      }
      case _ => WrongArgumentNumber(name, 2, args.length).failureNel
    }
  }

  object isNull extends StdFunction("null?") {
    def applyEvaled(args: List[SExpr], env: Env) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(Nil) => SBoolean(true).successNel
        case _ => SBoolean(false).successNel
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failureNel
    }
  }

  object list extends StdFunction("list") {
    def applyEvaled(args: List[SExpr], env: Env) = {
      if (!args.isEmpty) SList(args).successNel
      else WrongArgumentNumber(name, 1, args.length).failureNel
    }
  }

  object length extends StdFunction("length") {
    def applyEvaled(args: List[SExpr], env: Env) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(xs) => SNumber(xs.length).successNel
        case other => TypeMismatch("List", other.typeName).failureNel
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failureNel
    }
  }

  object define extends Function("define") {
    def apply(args: List[SExpr], env: Env) = args match {

      // (define var expr)
      case SSymbol(name) :: expr :: Nil => eval(env)(expr) match {
        case Success(value) => {
          env += (name -> value)
          value.successNel
        }
        case f @ Failure(_) => f
      }

      // (define (name param1 param2 ... paramN) body)
      case SList(SSymbol(name) :: params) :: body =>
        val lambda = SLambda(params.map(_.toString), None, body, env.extend)
        env += (name -> lambda)
        lambda.successNel

      // (define (name param1 param2 ... paramN . varArgs) body)
      case SDottedList(SSymbol(name) :: params, varargs) :: body =>
        val lambda = SLambda(params.map(_.toString), Some(varargs.toString), body, env.extend)
        env += (name -> lambda)
        lambda.successNel

      case (other :: _) => TypeMismatch("Symbol", other.typeName).failureNel
    }
  }

  object lambda extends Function("lambda") {
    def apply(args: List[SExpr], env: Env) = args match {

      // (lambda (param1 param2 ... paramN) body)
      case SList(params) :: body =>
        SLambda(params.map(_.toString), None, body, env.extend).successNel

      // (lambda (param1 param2 ... paramN . varargs) body)
      case SDottedList(params, varargs) :: body =>
        SLambda(params.map(_.toString), Some(varargs.toString), body, env.extend).successNel

      // (lambda varargs body)
      case varargs @ SSymbol(_) :: body =>
        SLambda(Nil, Some(varargs.toString), body, env.extend).successNel

      case expr => BadLambdaDef(expr).failureNel
    }
  }

  private val ops = List(add, diff, mul, div, eq, neq, lt, lte, gt, gte,
    sum, quote, ifStmt, car, cdr, cons, isNull, length,
    list, define, lambda)

  def builtins = ops.map(op => op.name -> op).toMap

  def defaultEnv = {
    val env = new Env
    env ++= builtins.map(b => (b._1, SNativeFunction(b._1, b._2)))
    env
  }

}
