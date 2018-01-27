package io.suppie.lcs.ga

import scala.annotation.switch
import scala.util.Random

class Reproduction[T <: Gene](score: Chromosome[T] => Unit) {
  private[this] val rand = new Random(System.currentTimeMillis)

  /**
    * Execute the 3 phases of the genetic replication: Selection, Cross-over and Mutation.
    *
    * @param population current population of chromosomes used in the replication process
    * @param config     configuration of the genetic algorithm.
    * @param cycle      Current reproduction cycle number
    * @return true if the selection, crossover and mutation phases succeed, None otherwise.
    */
  def mate(
            population: Population[T],
            config: GaConfig,
            cycle: Int): Boolean = (population.size: @switch) match {
    // If the population has less than 3 chromosomes, exit
    case 0 | 1 | 2 => false
    // Otherwise execute another reproduction cycle, starting with selection
    case _ =>
      rand.setSeed(rand.nextInt + System.currentTimeMillis)
      population.select(score, config.softLimit(cycle))
      population.crossover(rand.nextDouble * config.xover)
      population.mutation(rand.nextDouble * config.mu)
      true
  }
}


/**
  * Companion object for the Reproduction class. This singleton is used
  * to define the default constructor of the Reproduction class.
  *
  * @author Patrick Nicolas
  * @since August 28, 2013
  * @see Scala for Machine Learning Chapter 10 ''Genetic Algorithm'' / Genetic algorithm
  *      components
  */
object Reproduction {

  /**
    * Default constructor for a reproduction cycle
    *
    * @param score Scoring function of a chromosome (unfitness of the candidate solution)
    */
  def apply[T <: Gene](score: Chromosome[T] => Unit): Reproduction[T] = new Reproduction[T](score)
}