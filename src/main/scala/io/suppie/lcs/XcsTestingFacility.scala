package io.suppie.lcs

import io.suppie.lcs.GeneticEntities._

object XcsTestingFacility extends App {
  val multiplexerSizes = Array(
    Math.pow(2, 2).toInt,
    Math.pow(2, 3).toInt,
    Math.pow(2, 4).toInt,
    Math.pow(2, 5).toInt
  )

  val populationSizes = Array(
    50, 100, 200, 500, 1000, 1500, 2000, 5000
  )

  val trainingIterationsSizes = Array(
    10000, 20000, 30000, 40000, 50000
  )

  val experienceThresholds = Array(
    5, 10, 20, 30, 40, 50
  )

  // fitnessThreshold, beta, learningRate, alpha
  val zeroToOne = Array(
    0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.99
  )

  val newClassifierWildcardAppearanceRates = Array(
    1.0 / 2.0, 1.0 / 3.0, 1.0 / 4.0, 1.0 / 5.0, 1.0 / 10.0, 1.0 / 50.0, 1.0 / 100.0
  )

  val minError = Array(
    1.0, 2.0, 5.0, 10.0, 20.0, 50.0, 100.0, 200.0, 500.0, 1000.0, 2000.0, 5000.0, 10000.0
  )

  val vs = Array(
    10.0, 5.0, 2.0, 1.0, -1.0, -2.0, -5.0, -10.0
  )

  val geneticAlgorithmFrequencies = Array(
    1.0, 5.0, 10.0, 20.0, 50.0, 100.0
  )

  val firstParentSelectionStrategies = Array(
    RouletteWheelSelection, TournamentSelection(), TournamentSelection(5), TournamentSelection(10), TournamentSelection(20)
  )

  val secondParentSelectionStrategies = Array(
    Panmixic, PhenotypeInbreeding, PhenotypeOutbreeding, GenotypeInbreeding, GenotypeOutbreeding
  )

  val crossoverStrategies = Array(
    UniformCrossover, OnePointCrossover, NPointCrossover(), NPointCrossover(3)
  )

  val crossoverPredictionDecays = Array(
    2.0, 3.0, 4.0, 5.0, 10.0, 20.0, 50.0
  )

  val crossoverErrorDecay = Array(
    0.01, 0.05, 0.1, 0.125, 0.15, 0.2, 0.5, 0.99
  )

  val crossoverFitnessDecay = Array(
    0.01, 0.05, 0.1, 0.125, 0.15, 0.2, 0.5, 0.99
  )

  val mutationProbabilities = Array(
    0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08
  )

  val crates = Array(
    0.5, 0.6, 0.7, 0.8, 0.9, 1.0
  )

  val explorationFrequencies = Array(
    2, 3, 4, 5, 10, 20, 50, 100
  )

  val testRunsAmount = 100







  /*multiplexerSizes.indices foreach { j =>
    if (multiplexerSizes(j) != 4) {
      println(s"Multiplexer size: ${multiplexerSizes(j)}")

      populationSizes.indices foreach { x =>
        if (true) {
          println(s"Population size: ${populationSizes(x)}")

          trainingIterationsSizes.indices foreach { y =>
            if (true) {
              val test = Xcs(
                multiplexerSize = multiplexerSizes(j),
                populationSize = populationSizes(x),
                trainingIterations = trainingIterationsSizes(y)
              )

              println(s"Training instance size: ${trainingIterationsSizes(y)}")

              test.trainModel()

              println {
                (0 until testRunsAmount).map { i =>
                  test.testModel()
                }.mkString(",")
              }
              println()
            }
          }
          println()
        }
      }
      println()
    }
  }*/

  /*
  val test = Xcs()
  test.trainModel()
  test.testModel()
  */
}
