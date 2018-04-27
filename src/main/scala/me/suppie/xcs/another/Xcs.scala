package me.suppie.xcs.another

import java.util.{Random => JRandom}

import me.suppie.xcs.Entities.Rule
import me.suppie.xcs.generic.contracts.RuleInterpreter

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Xcs(
           ruleInterpreter: RuleInterpreter,
           environmentManager: EnvironmentManager,
           populationManager: PopulationManager,
           populationSize: Int = 200
         ) {
  final val CoinFlipProbability = 0.5

  private val rng: Random = Random.javaRandomToRandom(new JRandom(System.currentTimeMillis()))

  private val rulePopulation: ArrayBuffer[Rule] = {
    val population = ArrayBuffer.empty[Rule]
    population.sizeHint(populationSize)
    population
  }


}
