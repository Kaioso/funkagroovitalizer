package com.quadrifacet.funkagroovitalizer.formula

import scala.util.Random

/**
  * Created by howl on 4/9/2016.
  */
trait Term

class Value(val value: Int) extends Term

trait Operator extends Term {
  def operands: Int
  def precedence: Int
  def operate(values: List[Value]): Option[Value]
}

object OpenParenthesis extends Term
object CloseParenthesis extends Term
object NoOp extends Term

trait BinaryOperator extends Operator {
  def f: (Int, Int) => Int
  override val operands = 2
  override val precedence = 1
  override def operate(values: List[Value]) = values match {
    case v1 :: v2 :: Nil => Some(new Value(f(v1.value, v2.value)))
    case _ => None
  }
}

object Plus extends BinaryOperator { override def f = _ + _ }
object Minus extends BinaryOperator { override def f = _ - _ }
object Multiply extends BinaryOperator { override def f = _ * _; override val precedence = 2 }
object Divide extends BinaryOperator { override def f = _ / _; override val precedence = 2 }
object Dice extends BinaryOperator {
  override def f = (sides, times) => List.tabulate(times) { _ => new Random().nextInt(sides) + 1 }.sum
  override val precedence = 3 }