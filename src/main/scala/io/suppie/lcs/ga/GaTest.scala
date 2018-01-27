package io.suppie.lcs.ga

import io.suppie.lcs.ga.Chromosome.Pool
import io.suppie.lcs.ga.GaSolver
import io.suppie.lcs.trading.{GreaterThan, Signal, StrategyFactory}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}

object GaTest {
  private val CrossOverProbability = 0.8
  private val MutationProbability = 0.4
  private val MaxCycles = 1000 // Maximum number of iterations during the optimization
  private val CutOffSlope = -0.001 // Slope for the linear soft limit
  private val CutOffIntercept = 1.003 // Intercept value for the linear soft limit
  private val QuantizationRatio = 1024 // Quantization ratio for conversion Int <-> Double
  private val NumberOfFittest = 2 // Number of fittest chromosomes to consider as solution candidates

  private val SoftLimit = (n: Int) => CutOffSlope * n + CutOffIntercept
  private val NumSignalsPerStrategy = 3 // Number of trading signals per trading strategy (= number of genes in a chromosome)

  // Default data conversion
  implicit val digitize: Quantization = new Quantization(QuantizationRatio)
  implicit val encoding: Encoding = Gene.DefaultEncoding


  val relative = (xy: (Double, Double)) => xy._2 / xy._1 - 1.0
  val invRelative = (xy: (Double, Double)) => xy._1 / xy._2 - 1.0
  /**
    * Define the scoring function for the chromosomes (i.e. Trading strategies)
    * as the sum of the score of the genes (i.e. trading signals) in this chromosome (i.e strategy).
    */
  val scoring: Chromosome[Signal] => Unit = (chr: Chromosome[Signal]) => {
    val signals: List[Gene] = chr.code
    chr.cost = Math.log(signals.map(_.score).sum + 1.0)
  }

  /**
    * Monitoring function for the GA execution
    */
  private val averageCost = new ArrayBuffer[Double]
  private val tracker: Population[Signal] => Unit = (p: Population[Signal]) => averageCost.append(p.averageCost)


  /** Execution of the scalatest for '''GASolver''' class.
    * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate
    *
    * Exceptions thrown during the execution of the tests are caught by the wrapper or handler
    * test method in Eval trait defined as follows:
    * {{{
    *    def test(args: Array[String]) =
    *      Try(run(args)) match {
    *        case Success(n) => ...
    *        case Failure(e) => ...
    * }}}
    * The tests can be executed through ''sbt run'' or individually by calling
    * ''TestName.test(args)'' (i.e. DKalmanEval.test(Array[String]("IBM") )
    *
    * @param args array of arguments used in the test
    */
  def main(args: Array[String]): Unit = {
    println(s"Evaluation genetic algorithm")

    // Create trading strategies
    createStrategies.map(strategies => {
      println(s"\n${strategies.mkString("\n")}")

      // Initialize the population with a upper bound of 16 times
      // the initial number of strategies
      val initial = new Population[Signal](strategies.length << 4, strategies.head)

      println(s"${initial.symbolic()}")

      // Configure, instantiates the GA solver for trading signals
      val config = GaConfig(CrossOverProbability, MutationProbability, MaxCycles, SoftLimit)
      val solver = GaSolver[Signal](config, scoring, Some(tracker))


      // Extract the best population and the fittest chromosomes = trading strategies
      // from this final population.
      (solver |> initial).map(_.fittest.map(_.symbolic).getOrElse("NA")) match {
        case Success(results) => println(results)
        case Failure(e) => println(s"training ${e.getMessage}")
      }
    }).getOrElse(println("GAEval failed"))
  }

  /*
   * Create Trading strategies by loading price data from Yahoo financial tables.
   */
  private def createStrategies: Try[Array[Pool[Signal]]] = Try {
    val src = CsvCustomReader("ga_test_data.csv")

    val Cprice = src.getColumn[Double]("Adj Close")
    val CdPrice = deltaTest(Cprice, -1.0).get
    val Cvolume = src.getColumn[Double]("Volume")
    val CdVolume = deltaTest(Cvolume, 1.0).get
    val Cvolatility = src.getColumn[Double]("Volatility")
    val CdVolatility = deltaTest(Cvolatility, 1.0).get

    for {
      price <- Cprice
      dPrice <- CdPrice
      volume <- Cvolume
      dVolume <- CdVolume
      volatility <- Cvolatility
      dVolatility <- CdVolatility
    } yield {
      println(s"GS Stock price variation $dPrice")

      val factory = new StrategyFactory(NumSignalsPerStrategy)

      val avWeights = CdPrice.sum / CdPrice.size
      val weights = Vector.fill(CdPrice.size)(avWeights)
      factory += ("dVolume", 1.1, GreaterThan, CdVolume, weights)
      factory += ("volatility", 1.3, GreaterThan, Cvolatility.drop(1).toVector, weights)
      factory += ("dVolatility", 0.9, GreaterThan, CdVolatility, weights)

      factory.strategies
    }
  }

  def zipWithShift[T](xv: Array[T], n: Int): Array[(T, T)] = {
    require(n > 0 && n < xv.length, s"XTSeries.zipWithShift found shift n= $n required n < ${xv.length}")
    xv.drop(n).zip(xv.view.dropRight(n))
  }

  private def deltaTest(series: Array[Double], a: Double): Try[Vector[Double]] =
    Try(zipWithShift[Double](series, 1).map { case (x, y) => a * (y / x - 1.0) }.toVector)

  case class CsvCustomReader(path: String) {
    lazy val csv: Array[Array[String]] = load(path)
    lazy val header: Array[String] = csv.head

    private def load(path: String): Array[Array[String]] = {
      val bufferedSource = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(path))
      val result = (for (line <- bufferedSource.getLines()) yield line.split(",").map(_.trim)).toArray
      bufferedSource.close()
      result
    }

    def getColumn[T](h: String): Array[T] = {
      val idx = header.indexOf(h)
      if (idx < 0) throw new Exception("Column not found")
      csv.drop(1).map(array => array(idx).asInstanceOf[T]).toArray
    }
  }

}
