package io.suppie.lcs.another

import io.suppie.lcs.MultiplexerProvider
import io.suppie.lcs.another.Entities._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class AnotherXCS(
                       multiplexerSize: Int = 4,
                       populationSize: Int = 200,
                       deletionThreshold: Double = 20.0,
                       fitnessThreshold: Double = 0.1,
                       newClassifierWildcardAppearanceRate: Double = 0.5,
                       beta: Double,
                       minError: Double,
                       learningRate: Double,
                       alpha: Double,
                       v: Double
                     ) {
  type ActionType = Boolean

  val multiplexers = MultiplexerProvider(multiplexerSize)

  val rng: Random = Random(new java.util.Random(System.currentTimeMillis()))

  val population: ArrayBuffer[Classifier] = ArrayBuffer.empty
  population.sizeHint(populationSize)

  def deleteFromPopulation(): Unit = {
    val total = population.map(_.numerosity).sum

    if (total > populationSize) {
      def getDeletionVote(classifier: Classifier): Double = {
        val vote = classifier.setSize * classifier.numerosity
        val avgFitness = population.map(_.fitness / total).sum
        val derated = classifier.fitness / classifier.numerosity

        if (classifier.experience > deletionThreshold && derated < fitnessThreshold * avgFitness) {
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

  def getMatchingClassifiers(input: String, correctAnswer: ActionType, generation: Int): ArrayBuffer[Classifier] = {
    val matchingClassifiers = population.filter(rule => doesMatch(input, rule))

    if (matchingClassifiers.isEmpty) {
      val newClassifier = Classifier(
        condition = input.map(ch => if (rng.nextDouble() < newClassifierWildcardAppearanceRate) '#' else ch).mkString,
        action = correctAnswer,
        generation = generation
      )

      population += newClassifier
      matchingClassifiers += newClassifier
    }

    matchingClassifiers
  }

  def getPredictions(matchingClassifiers: ArrayBuffer[Classifier]): Array[Prediction] = {
    matchingClassifiers.groupBy(_.action).map { e =>
      val count = e._2.map(_.fitness).sum
      val sum = e._2.map(_.prediction).sum * count
      val weight = if (count > 0) sum / count else 0.0

      Prediction(e._1, sum, count, weight)
    }.toArray
  }

  def selectAction(predictions: Array[Prediction], isExplore: Boolean = false): ActionType = if (isExplore) {
    val actions = predictions.map(_.action)
    actions(rng.nextInt(actions.length))
  } else {
    predictions.maxBy(_.weight).action
  }

  def updateClassifiers(classifiers: ArrayBuffer[Classifier], reward: Double): Unit = {
    val numerositySum = classifiers.map(_.numerosity).sum

    classifiers.foreach { rule =>
      rule.experience = rule.experience + 1.0

      if (rule.experience < 1.0 / beta) {
        rule.error = (rule.error * (rule.experience - 1.0) + Math.abs(reward - rule.prediction)) / rule.experience
        rule.prediction = (rule.prediction * (rule.experience - 1.0) + reward) / rule.experience
        rule.setSize = (rule.setSize * (rule.experience - 1.0) + numerositySum) / rule.experience
      } else {
        rule.error = rule.error + beta * (Math.abs(reward - rule.prediction) - rule.error)
        rule.prediction = rule.prediction + beta * (reward - rule.prediction)
        rule.setSize = rule.setSize + beta * (numerositySum - rule.setSize)
      }
    }

    // Updating fitness
    var sum = 0.0

    classifiers.map { rule =>
      val result = if (rule.error < minError) 1.0 else Math.pow(alpha * (rule.error / minError), v)
      sum = sum + result * rule.numerosity
      (rule, result)
    }.foreach { e =>
      val rule = e._1
      rule.fitness = rule.fitness + learningRate * ((e._2 * rule.numerosity) / sum - rule.fitness)
    }
  }

  // TODO finish implementation
}
