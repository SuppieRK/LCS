package io.suppie.lcs.rl.environment

import java.{lang => jl, util => ju}

trait State[I <: java.io.Serializable] extends java.io.Serializable {
  def identifier: I
}

// For algorithm
case class TraceableState[T <: State[_]](state: T, seenTimes: Integer = 0) extends java.io.Serializable

// POMDP state and it's properties
case class Belief(identifier: ju.HashMap[String, jl.Double]) extends State[ju.HashMap[String, jl.Double]]