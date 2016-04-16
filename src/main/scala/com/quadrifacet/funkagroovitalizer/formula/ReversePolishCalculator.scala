package com.quadrifacet.funkagroovitalizer.formula

import scala.annotation.tailrec

/**
  * Created by howl on 4/9/2016.
  */
class ReversePolishCalculator {
  def calculate(terms: List[Term]): (Value, List[Value]) = {
    doCalculation(terms, Nil) match {
      case Nil => (new Value(0), Nil)
      case calculated => (calculated.headOption.getOrElse(new Value(0)), calculated.tail)
    }
  }

  @tailrec
  private def doCalculation(terms: List[Term], stack: List[Value]): List[Value] = {
    terms.headOption match {
      case Some(v: Value) => doCalculation(terms.tail, v :: stack)
      case Some(o: Operator) => doCalculation(terms.tail, operate(o, stack))
      case None => stack
    }
  }

  private def operate(o: Operator, stack: List[Value]): List[Value] = {
    val dropped = stack drop o.operands
    o.operate(stack take o.operands) map {v => v :: dropped} getOrElse dropped
  }
}
