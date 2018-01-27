package io.suppie.lcs.ga

import io.suppie.lcs.ga.state.GaMonitor

import scala.annotation.tailrec
import scala.util.Try

class GaSolver[T <: Gene](
                           config: GaConfig,
                           score: Chromosome[T] => Unit,
                           _monitor: Option[Population[T] => Unit]) extends GaMonitor[T] {
  type U = Population[T]
  type V = Population[T]

  protected val monitor: Option[Population[T] => Unit] = _monitor

  /**
    * Method to resolve any optimization problem using a function to generate
    * a population of Chromosomes, instead an existing initialized population
    *
    * @param initialize Function to generate the chromosomes of a population
    * @throws IllegalArgumentException If the initialization or chromosome generation
    *                                  function is undefined
    */
  def |>(initialize: () => Population[T]): Try[Population[T]] = this.|>(initialize())

  /**
    * This method leverages the genetic algorithm reproduction cycle to select the fittest
    * chromosomes (or solutions candidate) after a predefined number of reproduction cycles
    *
    * Convergence criteria used to end the reproduction cycle is somewhat dependent of the
    * problem or domain. This implementation makes sense for the exercise in the book
    * chapter 10. It needs to be modified to a specific application. This implementation relies on
    * the tail recursion
    *
    * @throws MatchError if the population is empty
    * @return PartialFunction with a parameterized population as input and the population
    *         containing the fittest chromosomes as output.
    */
  def |> : PartialFunction[U, Try[V]] = {
    case population: U if population.size > 1 && isReady =>

      start()
      // Create a reproduction cycle manager with a scoring function
      val reproduction = Reproduction[T](score)

      @tailrec
      def reproduce(population: Population[T], n: Int = 0): Population[T] =
        if (!reproduction.mate(population, config, n))
          population
        else if (isComplete(population, config.maxCycles - n))
          population
        else
          reproduce(population, n + 1)

      reproduce(population)

      // The population is returned no matter what..
      population.select(score, 1.0)
      Try(population)
  }
}


/**
  * Object companion for the Solve that defines the two constructors
  *
  * @author Patrick Nicolas
  * @since August 29, 2013
  * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
  */
object GASolver {
  final val GaCounter = "AvCost"

  /**
    * Default constructor for the Genetic Algorithm (class GASolver)
    *
    * @param config  Configuration parameters for the GA algorithm
    * @param score   Scoring method for the chromosomes of this population
    * @param monitor optional monitoring function that display the content or the various
    *                metric associated to the current population
    */
  def apply[T <: Gene](config: GaConfig, score: Chromosome[T] => Unit, monitor: Option[Population[T] => Unit]): GaSolver[T] =
    new GaSolver[T](config, score, monitor)

  /**
    * Default constructor for the Genetic Algorithm (class GASolver)
    *
    * @param config Configuration parameters for the GA algorithm
    * @param score  Scoring method for the chromosomes of this population
    */
  def apply[T <: Gene](config: GaConfig, score: Chromosome[T] => Unit): GaSolver[T] = new GaSolver[T](config, score, None)

  /**
    * Constructor for the Genetic Algorithm (class GASolver) with undefined scoring function
    *
    * @param config Configuration parameters for the GA algorithm
    */
  def apply[T <: Gene](config: GaConfig): GaSolver[T] = new GaSolver[T](config, (c: Chromosome[T]) => Unit, None)
}