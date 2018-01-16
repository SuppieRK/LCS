package io.suppie.lcs.rl.environment.pomdp

import java.util.Random
import java.{lang => jl, util => ju}

import io.suppie.lcs.rl.agent.{Agent, AgentConfig}
import io.suppie.lcs.rl.environment._

import scala.annotation.tailrec
import scala.collection.mutable

case class PomdpAgent(
                       envExecutor: PomdpEnvironmentExecutor,
                       fitnessPolicy: FitnessPolicy = StrictFitnessPolicy,
                       episodesAmount: Int = 100,
                       episodeLength: Int = 1000,
                       learningRate: Double = 0.8,
                       discountFactor: Double = 0.8,
                       beliefFitnessThreshold: Double = 1.0
                     ) extends Agent[ju.HashMap[String, jl.Double]] with AgentConfig {
  lazy val exploredSpace: mutable.Map[Belief, mutable.Map[Belief, RichConsequence]] = mutable.Map()
  lazy val rng: Random = new Random(System.currentTimeMillis())
  lazy val policy: ju.HashMap[ju.HashMap[String, jl.Double], Action] = generatePolicy

  override def getPolicy: ju.HashMap[ju.HashMap[String, jl.Double], Action] = policy

  private def generatePolicy: ju.HashMap[ju.HashMap[String, jl.Double], Action] = {
    validate()
    train()

    val result: ju.HashMap[ju.HashMap[String, jl.Double], Action] = new ju.HashMap

    exploredSpace.foreach(outer => {
      result.put(outer._1.identifier, outer._2.maxBy(inner => inner._2.estimate)._2.action)
    })

    result
  }

  private def train(): Unit = {
    0 to episodesAmount foreach { epoch =>
      traverse(TraceableState(findOrPutBelief(envExecutor.getAndSetRandomState)))
    }
  }

  @tailrec
  private def traverse(tState: TraceableState[Belief]): Unit = {
    if (tState.seenTimes < episodeLength) {
      // Set current belief state
      val currentBelief: Belief = tState.state

      // Get current belief state actions
      val availableActions: List[Action] = envExecutor.getAvailableActions

      // Get random action to take
      val nextAction: Action = availableActions(rng.nextInt(availableActions.size))

      // Execute action
      val newConsequence: Consequence = envExecutor.doAction(nextAction)

      // Get new state observation
      val newObservation: ju.HashMap[String, jl.Double] = envExecutor.getCurrentState

      // Get associated with new observation belief
      val newBelief: Belief = findOrPutBelief(newObservation)

      // Figure out the consequence for update
      val consequence: RichConsequence = if (exploredSpace(currentBelief).contains(newBelief)) {
        exploredSpace(currentBelief)(newBelief)
      } else {
        val c = RichConsequence(newConsequence)
        exploredSpace(currentBelief).put(newBelief, c)
        c
      }

      // Update quality
      val r: jl.Double = consequence.reward
      val q: jl.Double = consequence.quality
      val e: jl.Double = if (exploredSpace(newBelief).nonEmpty) {
        Option(exploredSpace(newBelief).maxBy(_._2.estimate)._2.estimate).getOrElse(0.0)
      } else {
        0.0
      }
      val lr: jl.Double = learningRate
      val df: jl.Double = discountFactor

      // Q-learning
      val newQuality = (1 - lr) * q + lr * (r + df * e)

      // Set new quality
      consequence.quality = newQuality

      if (!envExecutor.isCurrentStateGoal) {
        traverse(TraceableState(newBelief, tState.seenTimes + 1))
      }
    }
  }

  private def findOrPutBelief(observation: ju.HashMap[String, jl.Double]): Belief = {
    if (observation == null || observation.isEmpty) {
      null
    } else {
      if (exploredSpace.isEmpty) {
        val belief: Belief = Belief(observation)
        exploredSpace.put(belief, mutable.Map())
        belief
      } else {
        val researchedBeliefs = exploredSpace
          .keys
          .map(b => (b, fitnessPolicy.compare(b, observation)))
          .filter(bt => bt._2 > beliefFitnessThreshold)

        if (researchedBeliefs.isEmpty) {
          val belief: Belief = Belief(observation)
          exploredSpace.put(belief, mutable.Map())
          belief
        } else {
          val foundBelief = Option(researchedBeliefs.maxBy(_._2)._1)

          if (foundBelief.isDefined) {
            foundBelief.get
          } else {
            val belief: Belief = Belief(observation)
            exploredSpace.put(belief, mutable.Map())
            belief
          }
        }
      }
    }
  }
}
