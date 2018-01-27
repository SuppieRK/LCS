package io.suppie.lcs.ga.state

import io.suppie.lcs.ga.{Gene, Population}

import scala.util.Try

/**
  * Trait that specializes the generic monitoring capabilities of ''Monitor''
  * The main function of the monitor is to initialize and monitor the
  * state of the genetic algorithm. Monitoring is only available to the
  * ''GASolver.|>'' predictive method
  *
  * @tparam T type of the gene used in the genetic algorithm, with ''Gene'' as the
  *           type upper-bound
  * @author Patrick Nicolas
  * @version 0.99.1.1
  * @see Scala for Machine learning Chap 10 ''Genetic Algorithms'' / Implementation
  *      / Solver
  */
trait GaMonitor[T <: Gene] {
  self: {
    def |> : PartialFunction[Population[T], Try[Population[T]]]
  } =>

  /**
    * Function to monitor the state and attributes of the population of
    * chromosomes during the execution of the genetic algorithm
    */
  protected val monitor: Option[Population[T] => Unit]

  @volatile private var state: GaState = GaIdle()

  /**
    * Test of the genetic algorithm is ready to run
    *
    * @return true if GA is not running, false otherwise
    */
  @inline
  final def isReady: Boolean = state.isInstanceOf[GaIdle]

  /**
    * Initialize the state of the genetic algorithm as running
    */
  def start(): Unit = state.isInstanceOf[GaRunning]


  /**
    * Method that implements the exit condition for the execution of the
    * genetic algorithm. The method is responsible for implementing the
    * convergence criteria
    *
    * @param population      The current population of chromosomes
    * @param remainingCycles The number of cycles left in the execution of the
    *                        genetic algorithm
    * @return true if genetic algorithm is still running, false otherwise
    */
  def isComplete(population: Population[T], remainingCycles: Int): Boolean = {
    state = if (population.isEmpty)
      GaFailure(s"GASolver.converge failed at $remainingCycles cycle")
    else if (remainingCycles < 1)
      GaNoConvergence()
    else {
      monitor.foreach(_ (population))
      GaRunning()
    }

    !state.isInstanceOf[GaRunning]
  }
}
