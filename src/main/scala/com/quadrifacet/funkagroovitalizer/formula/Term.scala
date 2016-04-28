package com.quadrifacet.funkagroovitalizer.formula

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by howl on 4/9/2016.
  */
trait Term

class Value(val value: Int) extends Term {
  override def toString = value.toString
}

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
object Minus extends BinaryOperator { override def f = (a, b) => b - a }
object Multiply extends BinaryOperator { override def f = _ * _; override val precedence = 2 }
object Divide extends BinaryOperator { override def f = (a, b) => b / a; override val precedence = 2 }

class Dice(val multiResultHandler: Option[(List[Int] => Unit)]) extends BinaryOperator {
  override def f = (sides, times) => {
    val results = List.tabulate(times) { _ => new Random().nextInt(sides) + 1 }
    multiResultHandler foreach { h => h(results) }
    results.sum
  }
  override val precedence = 3
}

class Explode(val multiResultHandler: Option[(List[Int] => Unit)]) extends BinaryOperator {
  override def f = (sides, times) => {
    val results = roll(sides, times, Nil)
    multiResultHandler foreach { h => h(results) }
    results.sum
  }

  @tailrec
  private def roll(sides: Int, times: Int, stack: List[Int]): List[Int] = {
    val rolls = List.tabulate(times) { _ => new Random().nextInt(sides) + 1 }
    rolls.count(i => i == sides) match {
      case 0 => (rolls ++ stack).reverse
      case n => roll(sides, n, rolls ++ stack)
    }
  }

  override val precedence = 3
}