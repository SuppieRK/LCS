package io.suppie.lcs.rl.environment

import java.{lang => jl, util => ju}

trait EnvironmentExecutor[T <: java.io.Serializable] extends java.io.Serializable {
  def doAction(action: Action): Consequence

  def getAndSetRandomState: T

  def getCurrentState: T

  def isCurrentStateGoal: jl.Boolean

  def getAvailableActions: List[Action]
}

trait PomdpEnvironmentExecutor extends EnvironmentExecutor[ju.HashMap[String, jl.Double]]