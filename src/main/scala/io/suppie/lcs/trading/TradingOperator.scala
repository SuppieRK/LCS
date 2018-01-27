package io.suppie.lcs.trading

import io.suppie.lcs.ga.Operator

class TradingOperator(_id: Int) extends Operator {
  /**
    * Identifier (number) for this operator
    * * @return Number identifier
    */
  override def id: Int = _id

  /**
    * Create a new trading signal operator with a new identifier
    *
    * @param idx identifier for the operator
    * @return new trading signal operator
    */
  override def apply(idx: Int): TradingOperator = TradingOperator.Operators(idx)

  override def toString: String = id.toString
}

object None extends TradingOperator(0) {
  override def toString: String = "NA"
}

/**
  * Definition of the 'Lesser than' operator
  */
object LessThan extends TradingOperator(1) {
  override def toString: String = "<"
}

/**
  * Definition of the 'Greater than' operator
  */
object GreaterThan extends TradingOperator(2) {
  override def toString: String = ">"
}

/**
  * Definition of the 'equal' operator
  */
object Equal extends TradingOperator(3) {
  override def toString: String = "="
}

object TradingOperator {
  protected val Operators: Array[TradingOperator] = Array[TradingOperator](None, LessThan, GreaterThan, Equal)
}