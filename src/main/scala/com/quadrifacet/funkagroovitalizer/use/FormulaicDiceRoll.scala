package com.quadrifacet.funkagroovitalizer.use


/**
  * Created by howl on 4/1/2016.
  */
object FormulaicDiceRoll {
  def main(args: Array[String]) = {
//    for (i <- new FormulaicDiceRoll().execute()) println(i)
    println(new FormulaicDiceRoll().execute())
  }
}

class FormulaicDiceRoll {
  val formula = "1d20+4-(9+18/2 + (8+2)"

  def execute(): String = {
    val shunt = new ShuntingYard
    for (term <- tokenize()) {
      shunt.nextToken(term)
    }
    shunt.finish
  }

  private def tokenize(): Iterator[Term] = {
    val formulaMatcher = "((\\d+)|([+\\-*/de\\(\\)]))".r
    val matches = formulaMatcher findAllIn formula
    for (_ <- matches) yield {
      (2 to matches.groupCount map { i => matches group i }).toList match {
        case null :: "+" :: Nil => Operator("+", Plus)
        case null :: "-" :: Nil => Operator("-", Minus)
        case null :: "*" :: Nil => Operator("*", Multiply)
        case null :: "/" :: Nil => Operator("/", Divide)
        case null :: "d" :: Nil => Operator("d", Dice)
        case null :: "e" :: Nil => Operator("e", ExplodingDice)
        case null :: "(" :: Nil => OpenParenthesis("(")
        case null :: ")" :: Nil => CloseParenthesis(")")
        case digit :: null :: Nil => Value(digit)
        case _ => Value("0") // This shouldn't happen
      }
    }
  }
}

class ShuntingYard {
  var output: List[Term] = Nil
  var operators: List[Term] = Nil

  def nextToken(term: Term): Unit = {
    term match {
      case o: Value => output = o :: output
      case o: Operator => operators = dumpOperators(o, operators)
      case o: OpenParenthesis => operators = o :: operators
      case o: CloseParenthesis => operators = popTillOpenParenthesis(operators)
    }
  }

  def dumpOperators(o1: Operator, ops: List[Term]): List[Term] = {
    ops.headOption match {
      case Some(o2: Operator) if o1.op.precedence <= o2.op.precedence =>
        output = o2 :: output
        dumpOperators(o1, ops.tail)
      case None => o1 :: Nil
      case _ => o1 :: ops.tail
    }
  }

  def popTillOpenParenthesis(ops: List[Term]): List[Term] = {
    ops.headOption match {
      case Some(o: Operator) => output = o :: output
        popTillOpenParenthesis(ops.tail)
      case None => Nil
      case _ => ops.tail
    }
  }

  def finish: List[Term] = {
    for (o <- operators) {
      o match {
        case o1: Operator => output = o1 :: output
        case _ =>
      }
    }
    output.reverse
  }
}

abstract class Term
trait OperatorType {
  def operands: Int
  def precedence: Int
}

case class Value(repr: String) extends Term
case class Operator(repr: String, op: OperatorType) extends Term
case class OpenParenthesis(repr: String) extends Term
case class CloseParenthesis(repr: String) extends Term

object Plus extends OperatorType { override val operands = 2; override val precedence = 1 }
object Minus extends OperatorType { override val operands = 2; override val precedence = 1 }
object Multiply extends OperatorType { override val operands = 2; override val precedence = 2 }
object Divide extends OperatorType { override val operands = 2; override val precedence = 2 }
object Dice extends OperatorType { override val operands = 2; override val precedence = 3 }
object ExplodingDice extends OperatorType { override val operands = 2; override val precedence = 3 }