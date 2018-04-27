package me.suppie.xcs.generic

import java.util.{Random => JRandom}

import me.suppie.xcs.generic.contracts.impl.DefaultCoveringMechanism
import me.suppie.xcs.generic.contracts.{CoveringMechanism, LcsEnvironment, PopulationManager}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Main class of the Learning Classifier System algorithm
  * This is a Michigan-Style LCS (most traditional type of this algorithm)
  *
  * @param environment
  * @param populationSize
  * @tparam ES
  * @tparam A
  */
case class LcsLearningMachine[ES, A](
                                      environment: LcsEnvironment[ES, A],
                                      populationManager: PopulationManager[A],
                                      coveringMechanism: CoveringMechanism[A] = DefaultCoveringMechanism(),
                                      populationSize: Int = 200
                                    ) {
  private val rng = Random.javaRandomToRandom(new JRandom(System.currentTimeMillis()))

  private val rulePopulation = {
    val result = ArrayBuffer.empty[Classifier[A]]
    result.sizeHint(populationSize)
    result
  }

}
