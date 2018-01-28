package io.suppie.lcs.zdavep.mmc

import io.suppie.lcs.zdavep.genetic.Genetic.Gene

/**
  * Represents a number of coins.
  */
case class Change(n: Int, coin: Coin) extends Gene {
  def value: Double = n * coin.value

  override def copy: Change = Change(n, coin)

  override def toString: String = n + " " + coin.toString
}
