package io.suppie.lcs.rl.agent

import java.{util => ju}

import io.suppie.lcs.rl.environment.Action

trait Agent[T <: java.io.Serializable] extends java.io.Serializable {
  def getPolicy: ju.HashMap[T, Action]
}

trait AgentConfig {
  def episodesAmount: Int

  def episodeLength: Int

  def learningRate: Double

  def discountFactor: Double

  def beliefFitnessThreshold: Double

  def validate(): Unit = {
    if (episodesAmount < 0) throw new Exception(s"Episodes amount must be greater than zero, got $episodesAmount")
    if (episodeLength < 0) throw new Exception(s"Episode length must be greater than zero, got $episodeLength")
    if (learningRate < 0 || learningRate > 1) throw new Exception(s"Learning rate must be in range[0, 1], got $learningRate")
    if (discountFactor < 0 || discountFactor > 1) throw new Exception(s"Discount factor must be in range [0, 1], got $discountFactor")
    if (beliefFitnessThreshold < 0 || beliefFitnessThreshold > 1) throw new Exception(s"Belief fitness threshold must be in range [0, 1], got $beliefFitnessThreshold")
  }
}