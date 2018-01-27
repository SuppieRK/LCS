package io.suppie.lcs.trading

import io.suppie.lcs.ga.Chromosome.Pool
import io.suppie.lcs.ga.{Chromosome, Encoding, Quantization}

import scala.collection.mutable

/**
  * Trading Strategy defined as a list of trading signals. The signals are linked through
  * OR boolean operator IF( signal1 == true OR signal2 == true OR ...
  *
  * @constructor Create an instance of a trading strategy
  * @param name    Identifier or name of the strategy
  * @param signals List or sequence of trading signals used in this strategy.
  * @author Patrick Nicolas
  * @since 0.98.2 May 7, 2014
  * @version 0.99.1
  * @see Scale for Machine Learning Appendix/Finances 101
  */
case class TradingStrategy(name: String = "", signals: List[Signal]) {
  require(signals.nonEmpty, s"TradingStrategy The list of signals is undefined")
}


/**
  * Factory for trading strategies. The factory collects all the trading signals needed
  * to implement the trading strategy. The strategies are generated as the list of all
  * combination of nSignals trading signals, once and only once when requested. The Factory
  * is mainly used for initializing the population for the genetic algorithm or the
  * extended learning classifiers.
  *
  * @constructor Instantiate a factory for all the combination of nSignals trading signals.
  * @throws IllegalArgumentException if the number of signals is less than 1
  * @param nSignals Number of trading signals used in any trading strategy.
  * @param quant    Quantization function to convert signal to discrete value and vice versa
  * @author Patrick Nicolas
  * @since May 7, 2014
  * @note Scala for Machine Learning Chapter 10: Genetic Algorithms
  */
class StrategyFactory(nSignals: Int)(implicit quant: Quantization, encoding: Encoding) {
  require(nSignals > 0, s"StrategyFactory Number of signals $nSignals should be >0")

  private[this] val signals = new mutable.ListBuffer[Signal]

  /**
    * Create and add a new signal to the pool of this factory. The signal is defined by
    * its identifier, id, target value, operator, the observations its acts upon and optionally
    * the weights
    *
    * @param id      Identifier for the signal created and collected
    * @param target  target value (or threshold) for the signal created and collected
    * @param op      Operator of type SOperator of the signal added to the pool
    * @param obs     Observations or scalar time series used by the signal added to the pool
    * @param weights weights for the observations used by the signal (optional)
    */
  def +=(id: String, target: Double, op: TradingOperator, obs: Vector[Double], weights: Vector[Double]): Unit = {
    checkArguments(obs, weights)
    signals.append(Signal(id, target, op, obs, weights))
  }

  /**
    * Generates the trading strategies as any unique combinations of '''nSignals'''
    * of signals currently in the pool of signals. The list of strategies is computed on demand
    * only once (lazy value).
    *
    * @return strategies extracted from the pool of signals.
    */
  lazy val strategies: Pool[Signal] = {
    // Arbitrary ordering of signals for sorted tree set.
    implicit val ordered = Signal.orderedSignals

    val xss = new Pool[Signal]
    val treeSet = new mutable.TreeSet[Signal] ++= signals.toList
    val subsetsIterator = treeSet.subsets(nSignals)

    // Generates array of trading strategy by iterating
    // through the tree set.
    while (subsetsIterator.hasNext) {
      val subset = subsetsIterator.next()
      val signalList: List[Signal] = subset.toList
      xss.append(Chromosome[Signal](signalList))
    }
    xss
  }

  private def checkArguments(xt: Vector[Double], weights: Vector[Double]): Unit = {
    require(xt.nonEmpty,
      "StrategyFactory.checkArgument Input to this trading strategy is undefined")
    require(weights.nonEmpty,
      "StrategyFactory.checkArgument Input to this trading strategy is undefined")
  }
}