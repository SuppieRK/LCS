package io.suppie.lcs.ga

trait Operator {
  def id: Int = -1

  def apply(id: Int): Operator
}

object NullOperator extends Operator {
  override def id: Int = -1

  override def apply(id: Int): Operator = NullOperator
}