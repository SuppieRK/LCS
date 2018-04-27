package me.suppie.xcs

import java.text.NumberFormat

import me.suppie.xcs.Entities._
import me.suppie.xcs.GeneticEntities._
import me.suppie.xcs.multiplexer.MultiplexerProvider

import scala.collection.mutable.ArrayBuffer
import scala.util.{Random, Try}

/**
  * Classic eXtended Classifier System implementation (for multiplexer problem)
  *
  * @param multiplexerSize
  * @param populationSize
  * @param trainingIterations
  * @param testingIterations
  * @param performanceReportFrequency
  * @param experienceThreshold
  * @param fitnessThreshold
  * @param newClassifierWildcardAppearanceRate
  * @param beta
  * @param minError
  * @param learningRate
  * @param alpha
  * @param v
  * @param geneticAlgorithmFrequency
  * @param firstParentSelectionStrategy
  * @param secondParentSelectionStrategy
  * @param crossoverStrategy
  * @param crossoverPredictionDecay
  * @param crossoverErrorDecay
  * @param crossoverFitnessDecay
  * @param mutationProbability
  * @param crate
  * @param explorationFrequency
  * @param verbose
  */
case class Xcs(
                multiplexerSize: Int = 4,
                populationSize: Int = 200,
                trainingIterations: Int = 40000,
                testingIterations: Int = 10000,
                performanceReportFrequency: Int = 200,
                experienceThreshold: Double = 20.0,
                fitnessThreshold: Double = 0.1,
                newClassifierWildcardAppearanceRate: Double = 1.0 / 3.0,
                beta: Double = 0.2,
                minError: Double = 10.0,
                learningRate: Double = 0.2,
                alpha: Double = 0.1,
                v: Double = -5.0,
                geneticAlgorithmFrequency: Double = 50.0,
                firstParentSelectionStrategy: Selection = TournamentSelection(),
                secondParentSelectionStrategy: SecondParentSelection = Panmixic,
                crossoverStrategy: CrossoverType = UniformCrossover,
                crossoverPredictionDecay: Double = 2.0,
                crossoverErrorDecay: Double = 0.125,
                crossoverFitnessDecay: Double = 0.05,
                mutationProbability: Double = 0.04,
                crate: Double = 0.8,
                explorationFrequency: Int = 2,
                verbose: Boolean = false
              ) {
  // @TODO subsumption mechanism

  final val CoinFlipProbability: Double = 0.5

  val multiplexers = MultiplexerProvider(multiplexerSize)

  val rng: Random = Random.javaRandomToRandom(new java.util.Random(System.currentTimeMillis()))

  val population: ArrayBuffer[Classifier] = ArrayBuffer.empty
  population.sizeHint(populationSize)

  def trainModel(): Unit = {
    val performance: ArrayBuffer[Performance] = ArrayBuffer.empty

    val positiveReward: Double = 1000.0
    val negativeReward: Double = 0.0

    0 until trainingIterations foreach { generation =>
      val explore = generation % explorationFrequency == 0

      val input = multiplexers.generateRandomMultiplexerSignal
      val correctAction = multiplexers.targetFunction(input)

      val matchingClassifiers = getMatchingClassifiers(input, generation, training = true)
      val predictions = getPredictions(matchingClassifiers)
      val predictedAction = selectAction(predictions, explore)

      val reward = if (predictedAction.isDefined && correctAction == predictedAction.get) positiveReward else negativeReward

      if (explore) {
        updateClassifiers(matchingClassifiers.filter(rule => predictedAction.isDefined && rule.action == predictedAction.get), reward)
        runGeneticAlgorithm(matchingClassifiers, generation, input)
      } else if (verbose) {
        performance.append(Performance(
          error = Math.abs(predictions.maxBy(_.weight).weight - reward),
          correct = if (predictedAction.isDefined && correctAction == predictedAction.get) 1 else 0
        ))

        if (performance.size > performanceReportFrequency) {
          val error = Math.round(performance.map(_.error).sum / performance.length.toDouble)
          val accuracy = performance.map(_.correct).sum / performance.size.toDouble

          println(s"Generation $generation, population size ${population.length} => errors $error, accuracy ${accuracy.asPercentage}")

          performance.clear()
        }
      }
    }
  }

  def updateClassifiers(classifiers: ArrayBuffer[Classifier], reward: Double): Unit = if (classifiers.nonEmpty) {
    val numerositySum = classifiers.map(_.numerosity).sum

    classifiers.foreach { rule =>
      rule.experience = rule.experience + 1.0

      val (error, prediction, setSize) = if (rule.experience < 1.0 / beta) {
        (
          (rule.error * (rule.experience - 1.0) + Math.abs(reward - rule.prediction)) / rule.experience,
          (rule.prediction * (rule.experience - 1.0) + reward) / rule.experience,
          (rule.setSize * (rule.experience - 1.0) + numerositySum) / rule.experience
        )
      } else {
        (
          rule.error + beta * (Math.abs(reward - rule.prediction) - rule.error),
          rule.prediction + beta * (reward - rule.prediction),
          rule.setSize + beta * (numerositySum - rule.setSize)
        )
      }

      rule.error = error
      rule.prediction = prediction
      rule.setSize = setSize
    }

    // Updating fitness
    var sum = 0.0

    classifiers.map { rule =>
      val result = if (rule.error < minError) 1.0 else alpha * Math.pow(rule.error / minError, v)
      sum = sum + result * rule.numerosity
      (rule, result)
    }.foreach(e => e._1.fitness = e._1.fitness + learningRate * ((e._2 * e._1.numerosity) / sum - e._1.fitness))
  }

  def runGeneticAlgorithm(classifiers: ArrayBuffer[Classifier], generation: Int, input: String): Unit = if (classifiers.length > 2) {
    val total = classifiers.map(r => r.generation * r.numerosity).sum
    val sum = classifiers.map(_.numerosity).sum

    if (generation - (total / sum) > geneticAlgorithmFrequency) {
      // Parents selection
      val firstParent = firstParentSelectionStrategy match {
        case RouletteWheelSelection => rouletteWheelSelection(classifiers)
        case TournamentSelection(size) => tournamentSelection(classifiers, size)
      }

      val secondParent = secondParentSelectionStrategy match {
        case Panmixic => firstParentSelectionStrategy match {
          case RouletteWheelSelection => rouletteWheelSelection(classifiers -= firstParent)
          case TournamentSelection(size) => tournamentSelection(classifiers -= firstParent, size)
        }
        case PhenotypeInbreeding => getSecondParentPhIn(classifiers, firstParent)
        case PhenotypeOutbreeding => getSecondParentPhOut(classifiers, firstParent)
        case GenotypeInbreeding => getSecondParentGeIn(classifiers, firstParent)
        case GenotypeOutbreeding => getSecondParentGeOut(classifiers, firstParent)
      }

      // Crossover
      var (offspring1, offspring2) = if (Random.nextDouble() < crate) {
        crossover(firstParent, secondParent, generation)
      } else {
        (firstParent.copy(), secondParent.copy())
      }

      // Mutation
      offspring1 = mutation(offspring1, input)
      offspring2 = mutation(offspring2, input)

      // Insertion
      insertIntoPopulation(offspring1)
      insertIntoPopulation(offspring2)

      while (populationSize < population.map(_.numerosity).sum) {
        deleteFromPopulation()
      }
    }
  }

  def insertIntoPopulation(classifier: Classifier): Unit = {
    population.find(rule => rule.condition == classifier.condition && rule.action == classifier.action) match {
      case Some(r) => r.numerosity = r.numerosity + 1
      case None => population.append(classifier)
    }
  }

  // Genetic operations
  // Panmictic selection
  def rouletteWheelSelection(classifiers: ArrayBuffer[Classifier]): Classifier = {
    val totalFitness: Double = population.map(_.fitness).sum

    val point: Double = rng.nextDouble()

    population.map(c => (c, c.fitness / totalFitness))
      .find(_._2 > point).map(_._1)
      .getOrElse(population.maxBy(_.fitness / totalFitness))
  }

  // Preferred
  // Tournament selection
  def tournamentSelection(classifiers: ArrayBuffer[Classifier], tournamentSize: Int = 2): Classifier = {
    Random.shuffle(classifiers.indices.toList).take(tournamentSize).map(idx => classifiers(idx)).maxBy(_.fitness)
  }

  // Phenotype inbreeding for second parent selection
  def getSecondParentPhIn(classifiers: ArrayBuffer[Classifier], firstParent: Classifier): Classifier = {
    phenotypeSimilarity(classifiers, firstParent).minBy(_._2)._1
  }

  // Additional selection operators
  def phenotypeSimilarity(classifiers: ArrayBuffer[Classifier], firstParent: Classifier): ArrayBuffer[(Classifier, Double)] = {
    classifiers.filterNot(_ == firstParent).map(c => (c, Math.abs(c.fitness - firstParent.fitness)))
  }

  // Genotype inbreeding for second parent selection
  def getSecondParentGeIn(classifiers: ArrayBuffer[Classifier], firstParent: Classifier): Classifier = {
    genotypeSimilarity(classifiers, firstParent).maxBy(_._2)._1
  }

  // Phenotype outbreeding for second parent selection
  def getSecondParentPhOut(classifiers: ArrayBuffer[Classifier], firstParent: Classifier): Classifier = {
    phenotypeSimilarity(classifiers, firstParent).maxBy(_._2)._1
  }

  // Genotype outbreeding for second parent selection
  def getSecondParentGeOut(classifiers: ArrayBuffer[Classifier], firstParent: Classifier): Classifier = {
    genotypeSimilarity(classifiers, firstParent).minBy(_._2)._1
  }

  def genotypeSimilarity(classifiers: ArrayBuffer[Classifier], firstParent: Classifier): ArrayBuffer[(Classifier, Double)] = {
    classifiers.filterNot(_ == firstParent).map { c =>
      (c, c.condition.zipWithIndex.map { ch =>
        if (ch._1 == firstParent.condition.charAt(ch._2)) {
          1.0
        } else if (ch._1 == '#' || firstParent.condition.charAt(ch._2) == '#') {
          0.5
        } else {
          0.0
        }
      }.sum)
    }
  }

  // Crossover
  def crossover(firstParent: Classifier, secondParent: Classifier, generation: Int): (Classifier, Classifier) = {
    val (oCondition1, oCondition2) = crossoverStrategy match {
      case OnePointCrossover => onePointCrossover(firstParent, secondParent)
      case NPointCrossover(n) => nPointCrossover(n, firstParent, secondParent)
      case UniformCrossover => (uniformCrossover(firstParent, secondParent), uniformCrossover(firstParent, secondParent))
    }

    val prediction = (firstParent.prediction + secondParent.prediction) / crossoverPredictionDecay
    val error = (firstParent.error + secondParent.error) * crossoverErrorDecay
    val fitness = (firstParent.fitness + secondParent.fitness) * crossoverFitnessDecay

    (
      firstParent.copy(
        condition = oCondition1,
        generation = generation,
        prediction = prediction,
        error = error,
        fitness = fitness
      ),
      secondParent.copy(
        condition = oCondition2,
        generation = generation,
        prediction = prediction,
        error = error,
        fitness = fitness
      )
    )
  }

  // Preferred
  // Uniform crossover
  def uniformCrossover(firstParent: Classifier, secondParent: Classifier): String = {
    firstParent.condition.zipWithIndex.map { ch =>
      if (rng.nextDouble() < CoinFlipProbability) ch._1 else secondParent.condition.charAt(ch._2)
    }.mkString
  }

  // One point crossover
  def onePointCrossover(firstParent: Classifier, secondParent: Classifier): (String, String) = {
    val point = rng.nextInt(firstParent.condition.length)

    (
      firstParent.condition.take(point) + secondParent.condition.drop(point),
      secondParent.condition.take(point) + firstParent.condition.drop(point)
    )
  }

  // N point crossover
  def nPointCrossover(n: Int, firstParent: Classifier, secondParent: Classifier): (String, String) = {
    if (n >= firstParent.condition.length) {
      (uniformCrossover(firstParent, secondParent), uniformCrossover(firstParent, secondParent))
    } else {
      val bounds = List(0, firstParent.condition.length)
      val indicesToShuffle = firstParent.condition.indices.filterNot(bounds.contains)
      val shuffledIndices = (rng.shuffle(indicesToShuffle.toList).take(n) ++ bounds).sorted

      var fCondition = ""
      var sCondition = ""

      0 until (shuffledIndices.size - 1) foreach { i =>
        if (rng.nextDouble() < CoinFlipProbability) {
          fCondition = fCondition + firstParent.condition.substring(shuffledIndices(i), shuffledIndices(i + 1))
          sCondition = sCondition + secondParent.condition.substring(shuffledIndices(i), shuffledIndices(i + 1))
        } else {
          sCondition = sCondition + firstParent.condition.substring(shuffledIndices(i), shuffledIndices(i + 1))
          fCondition = fCondition + secondParent.condition.substring(shuffledIndices(i), shuffledIndices(i + 1))
        }
      }

      (fCondition, sCondition)
    }
  }

  // Mutation
  def mutation(classifier: Classifier, input: String): Classifier = {
    classifier.copy(
      condition = classifier.condition.zipWithIndex.map { ch =>
        if (rng.nextDouble() < mutationProbability) if (ch._1 == '#') input.charAt(ch._2) else '#' else ch._1
      }.mkString,
      action = if (rng.nextDouble() < mutationProbability) !classifier.action else classifier.action
    )
  }

  def testModel(): Double = {
    val result = (0 until trainingIterations).map { iteration =>
      val input = multiplexers.generateRandomMultiplexerSignal
      val matchingClassifiers = getMatchingClassifiers(input, iteration)
      val predictions = getPredictions(matchingClassifiers)
      val predictedAction = selectAction(predictions)

      if (predictedAction.isDefined && multiplexers.targetFunction(input) == predictedAction.get) {
        1
      } else {
        0
      }
    }.sum.toDouble / trainingIterations.toDouble

    if (verbose) println(s"Classified correctly ${result.asPercentage} instances")

    result
  }

  def getMatchingClassifiers(input: String, generation: Int, training: Boolean = false): ArrayBuffer[Classifier] = {
    val matchingClassifiers = population.filter(rule => doesMatch(input, rule))

    if (matchingClassifiers.isEmpty && training) {
      val newClassifier = Classifier(
        condition = input.map(ch => if (rng.nextDouble() < newClassifierWildcardAppearanceRate) '#' else ch).mkString,
        action = multiplexers.targetFunction(input),
        generation = generation
      )

      population += newClassifier
      matchingClassifiers += newClassifier
      deleteFromPopulation()
    }

    matchingClassifiers
  }

  def deleteFromPopulation(): Unit = {
    val total = population.map(_.numerosity).sum

    if (total > populationSize) {
      def getDeletionVote(classifier: Classifier): Double = {
        val vote = classifier.setSize * classifier.numerosity
        val avgFitness = population.map(_.fitness / total).sum
        val derated = classifier.fitness / classifier.numerosity

        if (classifier.experience > experienceThreshold && derated < fitnessThreshold * avgFitness) {
          vote * (avgFitness / derated)
        } else {
          vote
        }
      }

      val votes = population.zipWithIndex.map(e => (e._1, e._2, getDeletionVote(e._1)))
      val point = rng.nextDouble() * votes.map(_._3).sum

      import scala.util.control.Breaks._

      breakable {
        votes.foldLeft(0.0) { (prev, curr) =>
          val result = prev + curr._3

          if (result >= point) {
            if (population(curr._2).numerosity > 1) {
              population(curr._2).numerosity = population(curr._2).numerosity - 1
            } else {
              population -= curr._1
            }
            break()
          }

          result
        }
      }
    }
  }

  def doesMatch(input: String, classifier: Classifier): Boolean = {
    input.zipWithIndex.forall(e => classifier.condition.charAt(e._2) == '#' || classifier.condition.charAt(e._2) == e._1)
  }

  def getPredictions(matchingClassifiers: ArrayBuffer[Classifier]): Array[Prediction] = if (matchingClassifiers.nonEmpty) {
    matchingClassifiers.groupBy(_.action).map { e =>
      val count = e._2.map(_.fitness).sum
      val sum = e._2.map(_.prediction).sum * count
      val weight = if (count > 0) sum / count else 0.0

      Prediction(e._1, sum, count, weight)
    }.toArray
  } else {
    Array.empty
  }

  def selectAction(predictions: Array[Prediction], isExplore: Boolean = false): Option[Boolean] = Try {
    if (isExplore) {
      val actions = predictions.map(_.action)
      actions(rng.nextInt(actions.length))
    } else {
      predictions.maxBy(_.weight).action
    }
  }.toOption

  implicit class DoubleAsPercentage(d: Double) {
    def asPercentage: String = NumberFormat.getPercentInstance.format(d)
  }

}
