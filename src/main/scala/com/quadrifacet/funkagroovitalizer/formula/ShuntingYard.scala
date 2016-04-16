package com.quadrifacet.funkagroovitalizer.formula

import scala.annotation.tailrec

/**
  * Created by howl on 4/9/2016.
  */
object ShuntingYard {
  def tokenize(formula: String): List[Term] = {
    val formulaMatcher = "((\\d+)|([+\\-*/de\\(\\)]))".r
    val matches = formulaMatcher findAllIn formula
    (for (_ <- matches) yield {
      (2 to matches.groupCount map { i => matches group i }).toList match {
        case null :: "+" :: Nil => Plus
        case null :: "-" :: Nil => Minus
        case null :: "*" :: Nil => Multiply
        case null :: "/" :: Nil => Divide
        case null :: "d" :: Nil => Dice
//        case null :: "e" :: Nil => Operator("e", 3, 2)
        case null :: "(" :: Nil => OpenParenthesis
        case null :: ")" :: Nil => CloseParenthesis
        case digit :: null :: Nil => new Value(digit.toInt)
        case _ => NoOp // This shouldn't happen
      }
    }).filter( t => t match { case NoOp => false; case _ => true } ).toList
  }
}

class ShuntingYard {

  def transform(tokens: List[Term]): List[Term] = shunt(tokens, Nil, Nil).reverse

  @tailrec
  private def shunt(tokens: List[Term], operators: List[Term], output: List[Term]): List[Term] = {
    tokens.headOption match {
      case Some(o: Value) => shunt(tokens.tail, operators, o :: output)
      case Some(o: Operator) =>
        val (operatorShuntedOutput, restOfOperators) = popPrecedence(o, operators, output)
        shunt(tokens.tail, restOfOperators, operatorShuntedOutput)
      case Some(OpenParenthesis) => shunt(tokens.tail, OpenParenthesis :: operators, output)
      case Some(CloseParenthesis) =>
        val (operatorShuntedOutput, restOfOperators) = popParenthesis(operators, output)
        shunt(tokens.tail, restOfOperators, operatorShuntedOutput)
      case None => operators.reverse ++ output
    }
  }

  @tailrec
  private def popPrecedence(o1: Operator, operators: List[Term], output: List[Term]): (List[Term], List[Term]) = {
    operators.headOption match {
      case Some(o2: Operator) if o1.precedence <= o2.precedence => popPrecedence(o1, operators.tail, o2 :: output)
      case None => (output, o1 :: Nil)
      case _ => (output, o1 :: operators.tail)
    }
  }

  @tailrec
  private def popParenthesis(operators: List[Term], output: List[Term]): (List[Term], List[Term]) = {
    operators.headOption match {
      case Some(o: Operator) => popParenthesis(operators.tail, o :: output)
      case None => (output, Nil)
      case _ => (output, operators.tail)
    }
  }
}
