package io.suppie.lcs.trading

import io.suppie.lcs.ga.{Encoding, Gene, Operator, Quantization}

final class Signal(
                    id: String,
                    target: Double,
                    op: TradingOperator,
                    xt: Vector[Double],
                    weights: Vector[Double])(implicit quant: Quantization, encoding: Encoding)
  extends Gene(id, target, op) {

  import Signal._

  require(xt.nonEmpty, "Signal.check Cannot create a signal with undefined time series input")
  require(xt.size < MaxTimeSeriesSize,
    s"Signalcheck Size of the time series input, ${xt.size} if out of range")


  require(weights.nonEmpty, "Signal.check Cannot create a signal with undefined weights")
  require(weights.size < MaxTimeSeriesSize,
    s"Signalcheck Number of weights ${weights.size} if out of range")
  require(xt.size == weights.size,
    s"Signal The number of weights ${weights.size} is != size of data ${xt.size}")

  /**
    * Virtual constructor used in cloning, mutation and cross-over of gene, that
    * generate an instance of appropriate type.
    *
    * @param id     identifier for the signal
    * @param target Target values in the predicate/signal
    * @param op     Arithmetic or boolean operator used to trigger a signal from a value relative to
    *               the target
    * @return a new instance with target and operator modified through genetic reproduction but
    *         sharing the time series input xt and weights of its parent signal
    */
  override def toGene(id: String, target: Double, op: Operator): Gene =
    new Signal(id, target, op.asInstanceOf[TradingOperator], xt, weights)

  /**
    * Computation of the score of this trading signal by comparing a value with the threshold,
    * value.
    *
    * @return computed score for this trading signal
    */
  override def score: Double =
    if (!operatorFuncMap.contains(op))
      Double.MaxValue
    else
      sumScore(operatorFuncMap(op))


  /**
    * Compare this trading signal with another one
    *
    * @param that other trading signal
    * @return true if the two trading signals share the same operator and threshold value
    */
  def ==(that: Signal): Boolean = op == that.op && Math.abs(target - that.target) < Eps

  /**
    * Description of the trading signal using the encoded value of the target
    *
    * @return tuple (id, operator, encoded target value)
    */
  override def toString: String = s"$id ${op.toString} ${String.valueOf(target)}"

  private def sumScore(f: (Double, Double) => Double): Double = {
    xt.zip(weights).map { case (x, w) => w * f(x, target) }.sum
  }
}


/**
  * Companion object to the trading signal class, used to defined constructors.
  *
  * @author Patrick Nicolas
  * @note Scala for Machine Learning
  * @since March 4, 2014 Appendix Finances 101 / Technical analysis
  */
object Signal {
  private val Eps = 1e-3
  val CsvDelimiter = ","


  /**
    * Default constructor for Signal
    *
    * @param id      Label or identifier for the trading signal
    * @param target  Target value (or threshold) used to trigger the signal.
    * @param op      Operator that is used to defined the condition such as greater than, equals....
    * @param xt      Times series of single variable the signal acts upon.
    * @param weights Weights applied to each value of the time series (optional).
    * @param quant   Quantization function that convert analog or continuous signal to a
    *                discrete time series.
    */
  def apply(id: String, target: Double, op: TradingOperator, xt: Vector[Double], weights: Vector[Double])
           (implicit quant: Quantization, encoding: Encoding): Signal =
    new Signal(id, target, op, xt, weights)

  /**
    * Constructor for Signal with undefined weights and observations
    *
    * @param id     Label or identifier for the trading signal
    * @param target Target value (or threshold) used to trigger the signal.
    * @param op     Operator that is used to defined the condition such as greater than, equals....
    * @param quant  Quantization function that convert analog or continuous signal to a
    *               discrete time series.
    */
  def apply(id: String, target: Double, op: TradingOperator)(implicit quant: Quantization, encoding: Encoding): Signal =
    new Signal(id, target, op, Vector.empty[Double], Vector.empty[Double])

  /**
    * Define the ordering of a set of trading signals using the signal id.
    * Ordering is only used for generating unique trading strategies as
    * unique sequence of trading signals.
    */
  val orderedSignals: Ordering[Signal] = Ordering.by((signal: Signal) => signal.id)

  protected val operatorFuncMap: Map[TradingOperator, (Double, Double) => Double] = Map[TradingOperator, (Double, Double) => Double](
    LessThan -> ((x: Double, target: Double) => target - x),
    GreaterThan -> ((x: Double, target: Double) => x - target),
    Equal -> ((x: Double, target: Double) => Math.abs(x - target)),
    None -> ((x: Double, target: Double) => -1.0)
  )

  @inline final def numOperators: Int = operatorFuncMap.size

  private val MaxTimeSeriesSize: Int = 10000000
}