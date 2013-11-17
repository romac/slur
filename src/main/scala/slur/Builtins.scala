package slur

// TODO: Use implicits instead of explicit pack/unpackers.

import scalaz._
import Scalaz._
import Implicits._

trait Builtins { self: Runtime => 
  
  trait PackUnpack[A, B] { self: Operation =>
  
    val packer: B => SExpr
    val unpacker: SExpr => Validation[RuntimeError, A]
    
    protected def unpack(xs: List[SExpr]): Validation[RuntimeError, List[A]] = xs.map(unpacker).sequenceV
    protected def pack(value: B): SExpr = packer(value)
    
    def unpackError = s"Some arguments of '$name' are of unexpected type."
  }
  
  abstract class Operation(val name: String)
    extends (List[SExpr] => Validation[RuntimeError, SExpr]) {
    
    def applyEvaled(args: List[SExpr]): Validation[RuntimeError, SExpr]
    
    def apply(args: List[SExpr]): Validation[RuntimeError, SExpr] = for {
      evaled <- args.map(eval).sequenceV
      result <- applyEvaled(evaled)
    } yield result
  }
  
  abstract class RawOperation(name: String) extends Operation(name) {
    def applyEvaled(args: List[SExpr]) = ???
  }
  
  abstract class BinaryOperation[A, B](name: String)
    extends Operation(name) with PackUnpack[A, B] {
    
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
  
  abstract class FoldOperation[T, Z](name: String)
    extends Operation(name) with PackUnpack[T, Z] {
    
    val zero: Z
    def fold(acc: Z, elem: T): Z
    
    def applyEvaled(args: List[SExpr]): Validation[RuntimeError, SExpr] = args match {
      case Nil => pack(zero).success
      case x :: xs =>
        for (unpacked <- unpack(args)) yield pack(unpacked.foldLeft(zero)(fold))
    }
  }
  
  abstract class ReduceOperation[A](name: String)
    extends Operation(name) with PackUnpack[A, A] {
    
    val zero: A
    def reduce(a: A, b: A): A
    
    def applyEvaled(args: List[SExpr]): Validation[RuntimeError, SExpr] = args match {
      case Nil => pack(zero).success
      case x :: xs =>
        for (unpacked <- unpack(args)) yield pack(unpacked.reduce(reduce))
    }
  }
  
  object add extends ReduceOperation[Double]("+") {
    val zero = 0.0
    val packer = SNumber.apply _
    val unpacker = SNumber.unpack _
    def reduce(a: Double, b: Double) = a + b
  }
  
  object diff extends ReduceOperation[Double]("-") {
    val zero = 0.0
    val packer = SNumber.apply _
    val unpacker = SNumber.unpack _
    def reduce(a: Double, b: Double) = a - b
  }
  
  object mul extends ReduceOperation[Double]("*") {
    val zero = 1.0
    val packer = SNumber.apply _
    val unpacker = SNumber.unpack _
    def reduce(a: Double, b: Double) = a * b
  }
  
  object div extends BinaryOperation[Double, Double]("/") {
    val packer = SNumber.apply _
    val unpacker = SNumber.unpack _
    def op(a: Double, b: Double) = a / b
  }
  
  object eq extends Operation("=") {
    
    def applyEvaled(args: List[SExpr]) = { 
      if(args.length >= 2)
        SBoolean(args.tail.map(_ == args.head).forall(_ == true)).success
      else
        WrongArgumentNumber(name, 2, args.length).failure
    }
  }
  
  object quote extends RawOperation("quote") {
    
    override def apply(args: List[SExpr]) = args match {
      case x :: Nil => x.success
      case _ => WrongArgumentNumber(name, 1, args.length).failure 
    }
  }
  
  object ifStmt extends RawOperation("if") {
    
    override def apply(args: List[SExpr]) = args match {
      case pred :: conseq :: alt :: Nil => eval(pred).flatMap {
        case SBoolean(true) => eval(conseq)
        case SBoolean(false) => eval(alt)
        case other => TypeMismatch("boolean", other.typeName).failure
      }
      case _ => WrongArgumentNumber(name, 3, args.length).failure
    }
  }
  
  object car extends Operation("car") {
    
    def applyEvaled(args: List[SExpr]) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(x :: xs) => x.success
        case other => TypeMismatch("list", other.typeName).failure
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failure
    }
  }
  
  object cdr extends Operation("cdr") {
    
    def applyEvaled(args: List[SExpr]) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(x :: xs) => SList(xs).success
        case other => TypeMismatch("list", other.typeName).failure
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failure
    }
  }
  
  object cons extends Operation("cons") {
    
    def applyEvaled(args: List[SExpr]) = args match {
      case arg1 :: arg2 :: Nil => (arg1, arg2) match {
        case (x, SList(Nil)) => SList(x :: Nil).success
        case (x, SList(xs)) => SList(x :: xs).success
        case (x, y) => SList(x :: y :: Nil).success
      }
      case _ => WrongArgumentNumber(name, 2, args.length).failure
    }
  }
  
  object isNull extends Operation("null?") {
    def applyEvaled(args: List[SExpr]) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(Nil) => SBoolean(true).success
        case _ => SBoolean(false).success
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failure
    }
  }
  
  object list extends Operation("list") {
    def applyEvaled(args: List[SExpr]) = {
      if(!args.isEmpty) SList(args).success
      else WrongArgumentNumber(name, 1, args.length).failure
    }
  }
  
  object length extends Operation("length") {
    def applyEvaled(args: List[SExpr]) = args match {
      case arg1 :: Nil => arg1 match {
        case SList(xs) => SNumber(xs.length).success
        case other => TypeMismatch("list", other.typeName).failure
      }
      case _ => WrongArgumentNumber(name, 1, args.length).failure
    }
  } 
  
  val builtins: Map[String, Operation] = Map(
      add.name -> add,
      diff.name -> diff,
      mul.name -> mul,
      div.name -> div,
      eq.name -> eq,
      quote.name -> quote,
      ifStmt.name -> ifStmt,
      car.name -> car,
      cdr.name -> cdr,
      cons.name -> cons,
      isNull.name -> isNull,
      length.name -> length,
      list.name -> list
  )
  
}