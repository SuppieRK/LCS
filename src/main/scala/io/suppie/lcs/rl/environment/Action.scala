package io.suppie.lcs.rl.environment.action

import java.{lang, lang => jl}

sealed trait Action {
  def probability: jl.Double
}

case class StochasticAction(probability: jl.Double = 1.0) extends Action

case object DeterministicAction extends Action {
  override def probability: lang.Double = 1.0
}