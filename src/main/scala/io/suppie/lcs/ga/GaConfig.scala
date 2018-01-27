package io.suppie.lcs.ga

class GaConfig(val xover: Double, val mu: Double, val maxCycles: Int, val softLimit: Int => Double) {

  import GaConfig._

  require(maxCycles > 5 & maxCycles < DefaultMaxCycles, s"GaConfig Maximum number of iterations $maxCycles is out of bounds [0, MAX_CYCLES]")
  require(mu > 0.0 && mu < 1.0, s"GaConfig Mutation factor $mu is out of bounds [0, 1]")
  require(xover > 0.0 && xover < 1.0, s"GaConfig Crossover factor $xover is out of bounds [0, 1]")

  /**
    * re-compute the mutation factor using an attenuator
    *
    * @return soft limit computed for this cycle
    */
  @throws(classOf[IllegalArgumentException])
  val mutation: Int => Double = (cycle: Int) => {
    require(cycle >= 0 && cycle < maxCycles, s"GAConfig Iteration $cycle is out of range")
    softLimit(cycle)
  }

  /**
    * Textual representation of the configuration object
    */
  override def toString: String = s"Cross-over: $xover Mutation: $mu"
}


/**
  * Singleton that define the attenuator function for computing the cross-over or
  * mutation index of chromosomes, according the number of iterations in the genetic
  * algorithm optimization.
  *
  * @author Patrick Nicolas
  * @since August 28, 2013  (0.97)
  * @version 0.98.2
  * @see Scala for Machine Learning Chapter 10 Genetic Algorithm / implementation / GA
  *      configuration
  */
object GaConfig {
  private val DefaultSoftLimit: Int => Double = (n: Int) => -0.01 * n + 1.001
  private val DefaultMaxCycles: Int = 2048

  def apply(xover: Double, mu: Double, maxCycles: Int, softLimit: Int => Double): GaConfig = new GaConfig(xover, mu, maxCycles, softLimit)

  def apply(xover: Double, mu: Double, maxCycles: Int): GaConfig = new GaConfig(xover, mu, maxCycles, DefaultSoftLimit)
}