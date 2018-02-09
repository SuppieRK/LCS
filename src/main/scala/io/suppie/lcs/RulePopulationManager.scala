package io.suppie.lcs

import java.util.concurrent.ThreadLocalRandom

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
  * Michigan-Style (the most traditional style) Learning Classifier System
  *
  * @param maxNumberOfRules The maximum number of rules that can be contained in the population
  */
class RulePopulationManager(maxNumberOfRules: Int, multiplexerOrder: Int = 4) extends Matching with IntAdditionalLogic {
  // Rule population collectively forms the prediction model evolved by the LCS
  val rulePopulation: ArrayBuffer[Classifier] = ArrayBuffer.empty


  override def generateClassifier(
                                   input: String,
                                   actions: Array[Boolean],
                                   birthGeneration: Int,
                                   wildcardProbability: Double = 1 / (multiplexerOrder.exponentOfTwo + 1)
                                 ): Classifier = Classifier(
    condition = input.map(ch => if (ThreadLocalRandom.current().nextDouble() < wildcardProbability) '#' else ch).mkString,
    action = actions(ThreadLocalRandom.current().nextInt(actions.length)),
    birthGeneration = birthGeneration
  )

  override def doesMatch(input: String, condition: String): Boolean = {
    input.zipWithIndex.forall(e => condition.charAt(e._2) == '#' || condition.charAt(e._2) == e._1)
  }

  override def getMatchSet(input: String, allActions: Array[Boolean], generation: Int, deletionThreshold: Double = 20.0): ArrayBuffer[Classifier] = {
    val matchSet = rulePopulation.filter(r => doesMatch(input, r.condition))
    val actionsInMatchSet = matchSet.map(_.action).distinct

    while (actionsInMatchSet.length < allActions.length) {
      val remainingActions = allActions.filterNot(actionsInMatchSet.contains)
      val classifier = generateClassifier(input, remainingActions, generation)

      rulePopulation.append(classifier)
      matchSet.append(classifier)
      actionsInMatchSet.append(classifier.action)

      deleteFromPopulation(deletionThreshold)
    }

    matchSet
  }

  def deleteFromPopulation(deletionThreshold: Double): Unit = {
    val total = rulePopulation.map(_.numerosity).sum

    if (total > rulePopulation.length) {
      val votes = rulePopulation.zipWithIndex.map(e => (e._1, calculateDeletionVote(e._1, deletionThreshold), e._2))
      var votesSum = votes.map(_._2).sum
      val point = ThreadLocalRandom.current().nextDouble() * votesSum

      var index = 0
      votesSum = 0.0

      breakable {
        votes foreach { e =>
          votesSum = votesSum + e._2

          if (votesSum >= point) {
            index = e._3
            break()
          }
        }
      }

      if (rulePopulation(index).numerosity > 1) {
        rulePopulation(index).numerosity = rulePopulation(index).numerosity - 1
      } else {
        rulePopulation.remove(index)
      }
    }
  }

  def generatePrediction(matchSet: ArrayBuffer[Classifier]): Array[Prediction] = {
    matchSet.filterNot(_.action == null).groupBy(_.action).map { e =>
      val count = e._2.map(_.fitness).sum
      val sum = e._2.map(_.prediction).sum * count
      val weight = if (count > 0) sum / count else 0.0

      Prediction(e._1, sum, count, weight)
    }.toArray
  }

  def selectAction(predictions: Array[Prediction], pExplore: Boolean = false): Boolean = {
    val keys = predictions.map(_.action)

    if (pExplore) {
      keys(ThreadLocalRandom.current().nextInt(keys.length))
    } else {
      predictions.maxBy(_.weight).action
    }
  }

  def updateRules(reward: Double, beta: Double = 0.2): Unit = {
    val sum = rulePopulation.map(_.numerosity).sum

    rulePopulation foreach { rule =>
      rule.experience = rule.experience + 1

      if (rule.experience < 1.0 / beta) {
        rule.error = (rule.error * (rule.experience - 1.0) + Math.abs(reward - rule.prediction)) / rule.experience
        rule.prediction = (rule.prediction * (rule.experience - 1.0) + reward) / rule.experience
        rule.averageMatchSetSize = (rule.averageMatchSetSize * (rule.experience - 1.0) + sum) / rule.experience
      } else {
        rule.error = rule.error + beta * (Math.abs(reward - rule.prediction) - rule.error)
        rule.prediction = rule.prediction + beta * (reward - rule.prediction)
        rule.averageMatchSetSize + beta * (sum - rule.averageMatchSetSize)
      }
    }
  }

  def updateFitness(minError: Double = 10.0, learningRate: Double = 0.2, alpha: Double = 0.1, v: Double = -5.0): Unit = {
    var sum = 0.0

    rulePopulation.map { rule =>
      val result = if (rule.error < minError) 1.0 else Math.pow(alpha * (rule.error / minError), v)
      sum = sum + result * rule.numerosity
      (rule, result)
    }.foreach { e =>
      e._1.fitness = e._1.fitness + learningRate * ((e._2 * e._1.numerosity) / sum - e._1.fitness)
    }
  }

  private def calculateDeletionVote(
                                     classifier: Classifier,
                                     deletionThreshold: Double,
                                     fitnessThreshold: Double = 0.1
                                   ): Double = {
    val vote = classifier.averageMatchSetSize * classifier.numerosity
    val derated = classifier.fitness / classifier.numerosity

    val total = rulePopulation.map(_.numerosity).sum
    val avgFitness = rulePopulation.map(_.fitness / total).sum

    if (classifier.experience > deletionThreshold && derated < fitnessThreshold * avgFitness) {
      vote * (avgFitness / derated)
    } else {
      vote
    }
  }
}

/**
  * Rule is not a model - it is only a part of a model
  */
trait Rule {
  def condition: String

  def action: Boolean

  override def toString: String = s"IF $condition THEN $action"
}

case class Classifier(
                       condition: String,
                       action: Boolean,
                       birthGeneration: Int,
                       var numerosity: Double = 1.0,
                       var error: Double = 0.0,
                       var prediction: Double = 10.0,
                       var experience: Double = 0.0,
                       var fitness: Double = 10.0,
                       var averageMatchSetSize: Double = 1.0
                     ) extends Rule

case class Prediction(
                       action: Boolean,
                       sum: Double = 0.0,
                       count: Double = 0.0,
                       weight: Double = 0.0
                     )

case class Performance(
                        error: Double,
                        correct: Double
                      )

trait Covering {
  def generateClassifier(input: String, actions: Array[Boolean], birthGeneration: Int, wildcardProbability: Double): Classifier
}

trait Matching extends Covering {
  def doesMatch(input: String, condition: String): Boolean

  def getMatchSet(input: String, availableActions: Array[Boolean], generation: Int, deletionThreshold: Double): ArrayBuffer[Classifier]
}

class GeneticAlgorithm(rulePopulationManager: RulePopulationManager) {
  val population: ArrayBuffer[Classifier] = rulePopulationManager.rulePopulation

  def canRunGeneticAlgorithm(generation: Int, gaFreq: Double): Boolean = {
    if (population.length < 2) {
      false
    } else {
      val total = population.map(rule => rule.birthGeneration * rule.numerosity).sum
      val sum = population.map(_.numerosity).sum
      generation - (total / sum) > gaFreq
    }
  }

  def binaryTournament: Classifier = {
    val idxA = ThreadLocalRandom.current().nextInt(population.length)
    var idxB = ThreadLocalRandom.current().nextInt(population.length)

    while (idxA == idxB) {
      idxB = ThreadLocalRandom.current().nextInt(population.length)
    }

    if (population(idxA).fitness > population(idxB).fitness) population(idxA) else population(idxB)
  }

  def mutation(rule: Classifier, input: String, mutationRate: Double = 0.04): Unit = {
    val
  }
}
