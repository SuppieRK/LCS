package io.suppie.lcs.rl.environment

import java.{lang => jl}

case class Consequence(action: Action, reward: jl.Double) extends Serializable {
  @inline final def probability: jl.Double = action.probability
}

class RichConsequence(val action: Action, val reward: jl.Double, var quality: jl.Double = 0.0) extends Serializable {
  @inline final def estimate: jl.Double = probability * quality

  @inline final def probability: jl.Double = action.probability
}

object RichConsequence {
  def apply(action: Action, reward: jl.Double, quality: jl.Double = 0.0): RichConsequence = {
    new RichConsequence(action, reward, quality)
  }

  def apply(consequence: Consequence): RichConsequence = {
    new RichConsequence(consequence.action, consequence.reward, 0.0)
  }
}

