package com.quadrifacet.funkagroovitalizer.use


/**
  * Created by howl on 4/1/2016.
  */
class FormulaicDiceRoll {
  def execute(x: Term): Unit = {
    x match {
      case Operator(repr, op) =>

    }
  }
}

// var matches = "((\\d+)|([+-]))".r findAllIn("1+2-88")
// for (_ <- matches) { for (i <- 2 to matches.groupCount) { print("(" + i + ")" + matches.group(i) + ",") }; print(" ::\n"); }

abstract class Term
trait OperatorType {
  def operands: Int
  def precedence: Int
}

case class Value(repr: String) extends Term
case class Operator(repr: String, op: OperatorType) extends Term

class Plus extends OperatorType { override val operands = 2; override val precedence = 1 }