package io.suppie.lcs.rl.environment.pomdp

import java.{lang => jl, util => ju}

import io.suppie.lcs.rl.environment.Belief

trait FitnessPolicy {
  final def compare(b: Belief, newProps: ju.HashMap[String, jl.Double]): jl.Double = {
    if (b == null || newProps == null || ((b.identifier == null) && (newProps == null))) {
      0.0
    } else {
      iCompare(b, newProps)
    }
  }

  protected def iCompare(b: Belief, newProps: ju.HashMap[String, jl.Double]): jl.Double
}

object StrictFitnessPolicy extends FitnessPolicy {
  override protected def iCompare(b: Belief, newProps: ju.HashMap[String, jl.Double]): jl.Double = {
    if (b.identifier != null && b.identifier.equals(newProps)) 100.0 else 0.0
  }
}