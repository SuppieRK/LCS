package io.suppie.lcs.test

import java.text.NumberFormat

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks._

object Utils {
  def negate(bit: Byte): Byte = if (bit == 1) 0 else 1

  def targetFunction(input: String): Int = {
    val bytes = input.map(ch => if (ch == '0') 0.byteValue() else 1.byteValue())

    negate(bytes(0)) * negate(bytes(1)) * bytes(2) +
      negate(bytes(0)) * bytes(1) * bytes(3) +
      bytes(0) * negate(bytes(1)) * bytes(4) +
      bytes(0) * bytes(1) * bytes(5)
  }

  def copyClassifier(c: Classifier): Classifier = Classifier(
    condition = c.condition,
    action = c.action,
    generation = c.generation,
    prediction = c.prediction,
    error = c.error,
    fitness = c.fitness,
    setSize = c.setSize
  )

  def randomBitString(size: Int = 6): String = {
    0 until size map (_ => Math.round(Random.nextDouble())) mkString
  }

  def calculateDeletionVote(
                             classifier: Classifier,
                             population: ArrayBuffer[Classifier],
                             deletionThreshold: Double,
                             fitnessThreshold: Double = 0.1
                           ): Double = {
    val vote = classifier.setSize * classifier.numerocity
    val total = population.map(_.numerocity).sum
    val averageFitness = population.map(_.fitness / total).sum
    val derated = classifier.fitness / classifier.numerocity

    if (classifier.exp > deletionThreshold && derated < fitnessThreshold * averageFitness) {
      vote * (averageFitness / derated)
    } else {
      vote
    }
  }

  def deleteFromPopulation(population: ArrayBuffer[Classifier],
                           populationSize: Int,
                           deletionThreshold: Double = 20.0): Unit = {
    val total = population.map(_.numerocity).sum

    if (total > populationSize) {
      val votes: Seq[(Classifier, Double, Int)] = population.zipWithIndex.map(e => (e._1, calculateDeletionVote(e._1, population, deletionThreshold), e._2))
      var votesSum: Double = votes.map(_._2).sum
      val point = Random.nextDouble() * votesSum

      var index: Int = 0
      votesSum = 0.0

      breakable {
        votes.foreach { e =>
          votesSum = votesSum + e._2
          if (votesSum >= point) {
            index = e._3
            break()
          }
        }
      }

      if (population(index).numerocity > 1) {
        population(index).numerocity -= 1
      } else {
        population.remove(index)
      }
    }
  }

  def generateRandomClassifier(input: String,
                               actions: Array[Int],
                               generation: Int,
                               rate: Double = 1.0 / 3.0): Classifier = Classifier(
    condition = input.map(ch => if (Random.nextDouble() < rate) "#" else ch).mkString,
    action = actions(Random.nextInt(actions.length)),
    generation = generation
  )

  def doesMatch(input: String, condition: String): Boolean =
    input.zipWithIndex.forall(e => condition.charAt(e._2) == '#' || condition.charAt(e._2) == e._1)

  def getActions(population: ArrayBuffer[Classifier]): ArrayBuffer[Int] = population.map(_.action).distinct

  def generateMatchSet(input: String, population: ArrayBuffer[Classifier],
                       allActions: Array[Int], generation: Int, populationSize: Int): ArrayBuffer[Classifier] = {
    val matchSet = population.filter(c => doesMatch(input, c.condition))
    val actions = getActions(matchSet)

    while (actions.length < allActions.length) {
      val remaining = allActions.filter(!actions.contains(_))
      val classifier = generateRandomClassifier(input, remaining, generation)
      population += classifier
      matchSet += classifier
      deleteFromPopulation(population, populationSize)
      actions += classifier.action
    }

    matchSet
  }

  def generatePrediction(matchSet: ArrayBuffer[Classifier]): Array[Prediction] = {
    matchSet.filter(_.action != null).groupBy(_.action).map { e =>
      val count = e._2.map(_.fitness).sum
      val sum = e._2.map(_.prediction).sum * count
      val weight = if (count > 0) sum / count else 0.0

      Prediction(e._1, sum, count, weight)
    }.toArray
  }

  def selectAction(predictions: Array[Prediction], pExplore: Boolean = false): Int = {
    val keys = predictions.map(_.action)

    if (pExplore) {
      keys(Random.nextInt(keys.length))
    } else {
      predictions.maxBy(_.weight).action
    }
  }

  def updateSet(actionSet: ArrayBuffer[Classifier], reward: Double, beta: Double = 0.2): Unit = {
    val sum = actionSet.map(_.numerocity).sum

    actionSet.foreach { c =>
      c.exp += 1.0

      val prevError = c.error
      val prevPred = c.prediction
      val prevSetSize = c.setSize

      if (c.exp < 1.0 / beta) {
        c.error = (prevError * (c.exp - 1.0) + Math.abs(reward - prevPred)) / c.exp
        c.prediction = (prevPred * (c.exp - 1.0) + reward) / c.exp
        c.setSize = (prevSetSize * (c.exp - 1.0) + sum) / c.exp
      } else {
        c.error += beta * (Math.abs(reward - prevPred) - prevError)
        c.prediction += beta * (reward - prevPred)
        c.setSize += beta * (sum - prevSetSize)
      }
    }
  }

  def updateFitness(actionSet: ArrayBuffer[Classifier], minError: Double = 10.0,
                    learningRate: Double = 0.2, alpha: Double = 0.1, v: Double = -5.0): Unit = {
    var sum = 0.0

    actionSet.map { c =>
      val result = if (c.error < minError) 1.0 else Math.pow(alpha * (c.error / minError), v)
      sum += result * c.numerocity
      (c, result)
    }.foreach { e =>
      val old = e._1.fitness
      e._1.fitness += learningRate * ((e._2 * e._1.numerocity) / sum - old)
    }
  }

  def canRunGeneticAlgorithm(actionSet: ArrayBuffer[Classifier], gen: Double, gaFreq: Double): Boolean = {
    if (actionSet.length <= 2) {
      false
    } else {
      val total = actionSet.map(c => c.generation * c.numerocity).sum
      val sum = actionSet.map(_.numerocity).sum

      gen - (total / sum) > gaFreq
    }
  }

  def binaryTournament(population: ArrayBuffer[Classifier]): Classifier = {
    val i = population(Random.nextInt(population.length))
    var j = population(Random.nextInt(population.length))

    while (i == j) {
      j = population(Random.nextInt(population.length))
    }

    if (i.fitness > j.fitness) i else j
  }

  def mutation(c: Classifier, actionSet: ArrayBuffer[Classifier], input: String, rate: Double = 0.04): Unit = {
    val newCondition = c.condition.zipWithIndex.map { e =>
      if (Random.nextDouble() < rate) {
        if (e._1 == '#') {
          input.charAt(e._2)
        } else {
          '#'
        }
      } else {
        e._1
      }
    }.mkString

    c.condition = newCondition

    if (Random.nextDouble() < rate) {
      val subset = actionSet.filter(_ != c.action)
      c.action = subset(Random.nextInt(subset.size)).action
    }
  }

  def uniformCrossover(parent1: String, parent2: String): String = {
    parent1.zipWithIndex.map { e =>
      if (Random.nextDouble() < 0.5) e._1 else parent2.charAt(e._2)
    }.mkString
  }

  def insertInPopulation(cl: Classifier, population: ArrayBuffer[Classifier]): Unit = {
    population.find(c => c.condition == cl.condition && c.action == cl.action) match {
      case Some(classifier) =>
        classifier.numerocity += 1
      case None =>
        population += cl
    }
  }

  def crossover(c1: Classifier,
                c2: Classifier,
                p1: Classifier,
                p2: Classifier): Unit = {
    c1.condition = uniformCrossover(p1.condition, p2.condition)
    c2.condition = uniformCrossover(p1.condition, p2.condition)

    val pred = (p1.prediction + p2.prediction) / 2.0
    c1.prediction = pred
    c2.prediction = pred

    val error = 0.125 * (p1.error + p2.error)
    c1.error = error
    c2.error = error

    val fitness = 0.05 * (p1.fitness + p2.fitness)
    c1.fitness = fitness
    c2.fitness = fitness
  }

  def runGeneticAlgorithm(actions: Array[Int],
                          population: ArrayBuffer[Classifier],
                          actionSet: ArrayBuffer[Classifier],
                          input: String,
                          generation: Double,
                          populationSize: Int,
                          crate: Double = 0.8): Unit = {
    val p1 = binaryTournament(actionSet)
    val p2 = binaryTournament(actionSet)
    val c1 = copyClassifier(p1)
    val c2 = copyClassifier(p2)

    if (Random.nextDouble() < crate) {
      crossover(c1, c2, p1, p2)
    }

    Seq(c1, c2) foreach { c =>
      mutation(c, actionSet, input)
      insertInPopulation(c, population)
    }

    while (populationSize < population.map(_.numerocity).sum) {
      deleteFromPopulation(population, populationSize)
    }
  }

  def trainModel(populationSize: Int,
                 maxGenerations: Int,
                 actions: Array[Int],
                 gaFreq: Double,
                 inputSize: Int = 6): ArrayBuffer[Classifier] = {
    val population: ArrayBuffer[Classifier] = ArrayBuffer()
    val perf: ArrayBuffer[Performance] = ArrayBuffer()

    0 until maxGenerations foreach { generation =>
      val explore = generation % 2 == 0
      val input = randomBitString(inputSize)

      val matchSet = generateMatchSet(input, population, actions, generation, populationSize)
      val predArray = generatePrediction(matchSet)

      val action = selectAction(predArray, explore)

      val reward = if (targetFunction(input) == action) 1000.0 else 0.0

      if (explore) {
        val actionSet = matchSet.filter(_.action == action)
        updateSet(actionSet, reward)
        updateFitness(actionSet)

        if (canRunGeneticAlgorithm(actionSet, generation, gaFreq)) {
          actionSet.foreach(_.generation = generation)
          runGeneticAlgorithm(actions, population, actionSet, input, generation, populationSize)
        }
      } else {
        perf += Performance(
          Math.abs(predArray(action).weight - reward),
          if (reward == 1000.0) 1 else 0
        )

        if (perf.size >= 50) {
          val err = Math.round(perf.map(_.error).sum / perf.size)
          val acc = perf.map(_.correct).sum / perf.size

          println(s"Iteration ${generation + 1}, population size ${population.size}, error $err, accuracy $acc")

          perf.clear()
        }
      }
    }

    population
  }

  def testModel(system: ArrayBuffer[Classifier], numTrials: Int = 50,
                inputSize: Int = 6): Double = {
    val correct = (0 until numTrials).map { i =>
      val input = randomBitString(inputSize)

      val matchSet = system.filter(c => doesMatch(input, c.condition))

      val predArray = generatePrediction(matchSet)

      val action = selectAction(predArray)

      if (targetFunction(input) == action) {
        1
      } else {
        0
      }
    }.sum

    println(s"Done! Classified correctly ${(correct.toDouble / numTrials.toDouble).asPercentage}")

    correct.toDouble / numTrials.toDouble
  }

  def main(args: Array[String]): Unit = {
    val testLaunches = 100

    val system = trainModel(
      populationSize = 200,
      maxGenerations = 10000,
      actions = Array(0, 1),
      gaFreq = 25.0)

    println(s"Average accuracy is: ${
      ((0 until testLaunches).map { i =>
        testModel(system, 10000)
      }.sum / testLaunches.toDouble).asPercentage
    }")
  }

  implicit class DoubleAsPercentage(d: Double) {
    def asPercentage: String = NumberFormat.getPercentInstance.format(d)
  }

}

case class Performance(
                        error: Double,
                        correct: Double
                      )

case class Prediction(
                       action: Int,
                       sum: Double = 0.0,
                       count: Double = 0.0,
                       weight: Double = 0.0
                     )

case class Classifier(
                       var condition: String,
                       var action: Int,
                       var generation: Double,
                       var prediction: Double = 10.0,
                       var error: Double = 0.0,
                       var fitness: Double = 10.0,
                       var exp: Double = 0.0,
                       var setSize: Double = 1.0,
                       var numerocity: Double = 1.0
                     )
