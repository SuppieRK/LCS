package io.suppie.lcs.rl.environment

import java.{lang => jl}

trait Action extends Serializable {
  def probability: jl.Double
}

class StochasticAction(val probability: jl.Double = 1.0) extends Action

class DeterministicAction() extends Action {
  override def probability: jl.Double = 1.0
}