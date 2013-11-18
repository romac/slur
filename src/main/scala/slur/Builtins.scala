package slur

// TODO: Use implicits instead of explicit pack/unpackers.

import scalaz._
import Scalaz._
import Implicits._

trait Builtins { self: Runtime => 
  
  trait PackUnpack[A, B] { self: Function =>
  
    val packer: B => SExpr
    val unpacker: SExpr => Validation[RuntimeError, A]
    
    protected def unpack(xs: List[SExpr]): Validation[RuntimeError, List[A]] = xs.map(unpacker).sequenceV
    protected def pack(value: B): SExpr = packer(value)
    
    def unpackError = s"Some arguments of '$name' are of unexpected type."
  }
  
  abstract class Function(val name: String)
    extends (List[SExpr] => Validation[RuntimeError, SExpr]) {
    
    def apply(args: List[SExpr]): Validation[RuntimeError, SExpr]
  }
  
  abstract class StdFunction(name: String) extends Function(name) {
    
    def applyEvaled(args: List[SExpr]): Validation[RuntimeError, SExpr]
    
    def apply(args: List[SExpr]): Validation[RuntimeError, SExpr] = {
      for {
        evaled <- args.map(eval).sequenceV
        result <- applyEvaled(evaled)
      } yield result
    }
  }
  
  abstract class UnaryFunction[A, B](name: String)
    extends StdFunction(name) with PackUnpack[A, B] {
    
    def op(a: A): B
    
    def applyEvaled(args: List[SExpr]): Validation[RuntimeError, SExpr] = args match {
      case x :: Nil => for (a <- unpacker(x)) yield pack(op(a))
      case _ => WrongArgumentNumber(name, 1, args.length).failure 
    }
    
  }
  
  abstract class BinaryFunction[A, B](name: String)
    extends StdFunction(name) with PackUnpack[A, B] {
    
    def op(a: A, b: A): B
    
    def applyEvaled(args: List[SExpr]): Validation[RuntimeError, SExpr] = args match {
      case x :: y :: Nil =>
        for {
          a <- unpacker(x)
          b <- unpacker(y)
        } yield pack(op(a, b))
      case _ => WrongArgumentNumber(name, 2, args.length).failure 
    }
    
  }
  
  abstract class FoldFunction[T, Z](name: String)
    extends StdFunction(name) with PackUnpack[T, Z] {
    
    val zero: Z
    def fold(acc: Z, elem: T): Z
    
    def applyEvaled(args: List[SExpr]): Validation[RuntimeError, SExpr] = args match {
      case Nil => pack(zero).success
      case x :: xs =>
        for (unpacked <- unpack(args)) yield pack(unpacked.foldLeft(zero)(fold))
    }
  }
  
  abstract class ReduceFunction[A](name: String)
    extends StdFunction(name) with PackUnpack[A, A] {
    
    val zero: A
    def reduce(a: A, b: A): A
    
    def applyEvaled(args: List[SExpr]): Validation[RuntimeError, SExpr] = args match {
      case Nil => pack(zero).success
      case x :: xs =>
        for (unpacked <- unpack(args)) yield pack(unpacked.reduce(reduce))
    }
  }
  
  object add extends BinaryFunction[Double, Double]("+") {
    val packer = SNumber.apply _
    val unpacker = SNumber.unpack _
    def op(a: Double, b: Double) = a + b
  }
  
  object diff extends BinaryFunction[Double, Double]("-") {
    val packer = SNumber.apply _
    val unpacker = SNumber.unpack _
    def op(a: Double, b: Double) = a - b
  }
  
  object mul extends BinaryFunction[Double, Double]("*") {
    val packer = SNumber.apply _
    val unpacker = SNumber.unpack _
    def op(a: Double, b: Double) = a * b
  }
  
  object div extends BinaryFunction[Double, Double]("/") {
    val packer = SNumber.apply _
    val unpacker = SNumber.unpack _
    def op(a: Double, b: Double) = a / b
  }
  
  object neq extends BinaryFunction[Double, Boolean]("/=") {
    val packer = SBoolean.apply _
    val unpacker = SNumber.unpack _
    def op(a: Double, b: Double) = a != b
  }
  
  object gt extends BinaryFunction[Double, Boolean](">") {
    val packer = SBoolean.apply _
    val unpacker = SNumber.unpack _
    def op(a: Double, b: Double) = a > b
  }
  object gte extends BinaryFunction[Double, Boolean](">=") {
    val packer = SBoolean.apply _
    val unpacker = SNumber.unpack _
    def op(a: Double, b: Double) = a >= b
  }
  
  object lt extends BinaryFunction[Double, Boolean]("<") {
    val packer = SBoolean.apply _
    val unpacker = SNumber.unpack _
    def op(a: Double, b: Double) = a < b
  }
  object lte extends BinaryFunction[Double, Boolean]("<=") {
    val packer = SBoolean.apply _
    val unpacker = SNumber.unpack _
    def op(a: Double, b: Double) = a <= b
  }
  
  object eq extends BinaryFunction[Double, Boolean]("=") {
    val packer = SBoolean.apply _
    val unpacker = SNumber.unpack _
    def op(a: Double, b: Double) = a == b
  }
  
  object and extends BinaryFunction[Boolean, Boolean]("&&") {
    val packer = SBoolean.apply _
    val unpacker = SBoolean.unpack _
    def op(a: Boolean, b: Boolean) = a && b
  }
  
  object or extends BinaryFunction[Boolean, Boolean]("||") {
    val packer = SBoolean.apply _
    val unpacker = SBoolean.unpack _
    def op(a: Boolean, b: Boolean) = a || b
  }
  
  object sum extends ReduceFunction[Double]("sum") {
    val zero = 0.0
    val packer = SNumber.apply _
    val unpacker = SNumber.unpack _
    def reduce(a: Double, b: Double) = a + b
  }
  
  object quote extends Function("quote") {
    
    override def apply(args: List[SExpr]) = args match {
      case x :: Nil => x.success
      case _ => WrongArgumentNumber(name, 1, args.length).failure 
    }
  }
  
  object ifStmt extends Function("if") {
    
    override def apply(args: List[SExpr]) = args match {
      case pred :: conseq :: alt :: Nil => eval(pred).flatMap {
        case SBoolean(true) => eval(conseq)
        case SBoolean(false) => eval(alt)
        case other => TypeMismatch("boolean", other.typeName).failure
      }
      case _ => WrongArgumentNumber(name, 3, args.length).failure
    }
  }
  
  object car extends StdFunction("car") {
    
    def applyEvaled(args: List[SExpr]) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(x :: xs) => x.success
        case SDottedList(x :: xs, _) => x.success
        case other => TypeMismatch("list", other.typeName).failure
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failure
    }
  }
  
  object cdr extends StdFunction("cdr") {
    
    def applyEvaled(args: List[SExpr]) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(x :: xs) => SList(xs).success
        case SDottedList(List(_), x) => x.success
        case SDottedList(_ :: xs, x) => SDottedList(xs, x).success
        case other => TypeMismatch("list", other.typeName).failure
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failure
    }
  }
  
  object cons extends StdFunction("cons") {
    
    def applyEvaled(args: List[SExpr]) = args match {
      case arg1 :: arg2 :: Nil => (arg1, arg2) match {
        case (x, SList(Nil)) => SList(x :: Nil).success
        case (x, SList(xs)) => SList(x :: xs).success
        case (x, SDottedList(xs, xlast)) => SDottedList(x :: xs, xlast).success
        case (x, y) => SList(x :: y :: Nil).success
      }
      case _ => WrongArgumentNumber(name, 2, args.length).failure
    }
  }
  
  object isNull extends StdFunction("null?") {
    def applyEvaled(args: List[SExpr]) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(Nil) => SBoolean(true).success
        case _ => SBoolean(false).success
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failure
    }
  }
  
  object list extends StdFunction("list") {
    def applyEvaled(args: List[SExpr]) = {
      if(!args.isEmpty) SList(args).success
      else WrongArgumentNumber(name, 1, args.length).failure
    }
  }
  
  object length extends StdFunction("length") {
    def applyEvaled(args: List[SExpr]) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(xs) => SNumber(xs.length).success
        case other => TypeMismatch("List", other.typeName).failure
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failure
    }
  }
  
  object define extends Function("define") {
    def apply(args: List[SExpr]) = args match {
      case x :: y :: Nil => (x, eval(y)) match {
        case (SSymbol(name), Success(value)) => {
          env += (name -> value)
          value.success
        }
        case (_, Failure(f)) => f.failure
        case (other, _) => TypeMismatch("Symbol", other.typeName).failure
      }
      case _ => WrongArgumentNumber(name, 2, args.length).failure
    }
  }
  
  val builtins: Map[String, Function] = Map(
      add.name -> add,
      diff.name -> diff,
      mul.name -> mul,
      div.name -> div,
      eq.name -> eq,
      neq.name -> neq,
      lt.name -> lt,
      lte.name -> lte,
      gt.name -> gt,
      gte.name -> gte,
      sum.name -> sum,
      quote.name -> quote,
      ifStmt.name -> ifStmt,
      car.name -> car,
      cdr.name -> cdr,
      cons.name -> cons,
      isNull.name -> isNull,
      length.name -> length,
      list.name -> list,
      define.name -> define
  )
  
}