package com.quadrifacet.funkagroovitalizer.use

import com.quadrifacet.funkagroovitalizer.formula.{ReversePolishCalculator, ShuntingYard}


/**
  * Created by howl on 4/1/2016.
  */
object FormulaicDiceRoll {
  def main(args: Array[String]) = {
    println(new FormulaicDiceRoll().execute())
  }
}

class FormulaicDiceRoll {
//  val formula = "20+4-(9+18)/2 + (8+2)"
  val formula = "5d1+5"
  def execute(): Int = {
    val (result, extra) = new ReversePolishCalculator().calculate(new ShuntingYard().transform(ShuntingYard.tokenize(formula)))
    result.value
  }
}