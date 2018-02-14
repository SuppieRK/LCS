package io.suppie.lcs.newlcs

import java.text.NumberFormat

import io.suppie.lcs.newlcs.SelectionStrategy._
import io.suppie.lcs.{IntAdditionalLogic, MultiplexerProvider}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class Classifier(
                       condition: String,
                       action: Boolean,
                       birthGeneration: Int,
                       var numerosity: Int = 1,
                       var experience: Double = 0,
                       var error: Double = 0,
                       var prediction: Double = 0.0,
                       var fitness: Double = 0.01,
                       var setSize: Double = 0
                     ) {
  override def toString = s"Classifier(condition=$condition, action=$action, birthGeneration=$birthGeneration, numerosity=$numerosity, experience=$experience, error=$error, prediction=$prediction, fitness=$fitness, setSize=$setSize)"
}

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

object SelectionStrategy {

  trait Selection

  case class TournamentSelection(size: Int = 2) extends Selection

  case object RouletteWheelSelection extends Selection

  trait SecondParentSelection

  case object Panmixic extends SecondParentSelection

  case object PhenotypeInbreeding extends SecondParentSelection

  case object PhenotypeOutbreeding extends SecondParentSelection

  case object GenotypeInbreeding extends SecondParentSelection

  case object GenotypeOutbreeding extends SecondParentSelection

  trait CrossoverType

  case object OnePointCrossover extends CrossoverType

  case class NPointCrossover(n: Int) extends CrossoverType

  case object UniformCrossover extends CrossoverType

}

case class XCS(
                multiplexerSize: Int = 4,
                trainSetSize: Int = 40000,
                testSetSize: Int = 10000,
                runTimes: Int = 100,
                populationSize: Int = 200,
                coveringWildcardProbability: Double = 1.0 / 3.0,
                gaFirstParentSelection: Selection = TournamentSelection(),
                gaSecondParentSelection: SecondParentSelection = Panmixic,
                gaCrossoverType: CrossoverType = UniformCrossover,
                crossoverProbability: Double = 0.5,
                crossoverPredictionDecay: Double = 2.0,
                crossoverErrorDecay: Double = 0.125,
                crossoverFitnessDecay: Double = 0.05,
                mutationProbability: Double = 0.04,
                deletionThreshold: Double = 20.0,
                fitnessThreshold: Double = 0.1,
                beta: Double = 0.2,
                minError: Double = 10.0,
                learningRate: Double = 0.2,
                alpha: Double = 0.1,
                v: Double = -0.5,
                crate: Double = 0.8,
                gaFrequency: Double = 10.0,
                explorationRate: Int = 2
              ) extends IntAdditionalLogic {
  val multiplexers = MultiplexerProvider(multiplexerSize)

  val population: ArrayBuffer[Classifier] = ArrayBuffer.empty

  def insertRule(cl: Classifier): Unit = {
    population.find(c => c.condition == cl.condition && c.action == cl.action) match {
      case Some(classifier) => classifier.numerosity = classifier.numerosity + 1
      case None => population.append(cl)
    }
  }

  def deleteFromPopulation(): Unit = {
    val total = population.map(_.numerosity).sum

    def calculateDeletionRule(cl: Classifier): Double = {
      val vote = cl.setSize / cl.numerosity
      val avgFitness = population.map(_.fitness / total).sum
      val derated = cl.fitness / cl.numerosity

      if (cl.experience > deletionThreshold && derated < fitnessThreshold * avgFitness) {
        vote * (avgFitness / derated)
      } else {
        vote
      }
    }

    if (total > populationSize) {
      val votes = population.zipWithIndex.map(c => (c._1, calculateDeletionRule(c._1), c._2))
      var point = Random.nextDouble() * votes.map(_._2).sum

      import scala.util.control.Breaks._

      var votesSum = 0.0
      var index = 0

      breakable {
        votes.foreach { vote =>
          votesSum = votesSum + vote._2

          if (votesSum >= point) {
            index = vote._3
            break()
          }
        }
      }

      if (population(index).numerosity > 1) {
        population(index).numerosity = population(index).numerosity - 1
      } else {
        population -= population(index)
      }
    }
  }

  // 1. Matching
  def doesMatch(input: String, rule: String): Boolean = {
    input.zipWithIndex.forall(e => rule.charAt(e._2) == '#' || rule.charAt(e._2) == e._1)
  }

  // 2. Generate match set
  // 3. Split match set onto correct and incorrect sets
  def getMatchSet(input: String, possibleActions: Array[Boolean], currentGenerationIndex: Int): ArrayBuffer[Classifier] = {
    val correctSet = population.filter(rule => doesMatch(input, rule.condition))

    if (correctSet.isEmpty) {
      val coveringClassifier = coveringMechanism(input, possibleActions(Random.nextInt(possibleActions.length)), currentGenerationIndex)
      correctSet.append(coveringClassifier)
      population.append(coveringClassifier)
      deleteFromPopulation()
    }

    correctSet
  }

  // 4. Covering mechanism aka Rule discovery
  def coveringMechanism(input: String, correctAction: Boolean, currentGenerationIndex: Int): Classifier = {
    Classifier(
      condition = input.map(ch => if (Random.nextDouble() < coveringWildcardProbability) '#' else ch).mkString,
      action = correctAction,
      birthGeneration = currentGenerationIndex
    )
  }

  // 5. Update parameter classifiers
  def updateSet(set: ArrayBuffer[Classifier], reward: Double): Unit = if (set.nonEmpty) {
    val numerositySum = set.map(_.numerosity).sum

    // Updating general parameters
    set.foreach { rule =>
      val prevExperience = rule.experience
      rule.experience = prevExperience + 1.0

      if (rule.experience < 1.0 / beta) {
        rule.error = (rule.error * prevExperience + Math.abs(reward - rule.prediction)) / rule.experience
        rule.prediction = (rule.prediction * prevExperience + reward) / rule.experience
        rule.setSize = (rule.setSize * prevExperience + numerositySum) / rule.experience
      } else {
        rule.error = rule.error + beta * (Math.abs(reward - rule.prediction) - rule.error)
        rule.prediction = rule.prediction + beta * (reward - rule.prediction)
        rule.setSize = rule.setSize + beta * (numerositySum - rule.setSize)
      }
    }

    // Updating fitness
    var sum = 0.0

    set.map { rule =>
      val result = if (rule.error < minError) 1.0 else Math.pow(alpha * (rule.error / minError), v)
      sum = sum + result * rule.numerosity
      (rule, result)
    }.foreach { e =>
      val rule = e._1
      rule.fitness = rule.fitness + learningRate * ((e._2 * rule.numerosity) / sum - rule.fitness)
    }
  }

  // 6. Subsumption is left for the sake of GA subsumption

  // 7. Genetic algorithm
  def canRunGa(generation: Int, correctSet: ArrayBuffer[Classifier]): Boolean = {
    if (correctSet.length > 2) {
      val total = correctSet.map(c => c.birthGeneration * c.numerosity).sum
      val sum = correctSet.map(_.numerosity).sum
      generation - (total / sum) > gaFrequency
    } else {
      false
    }
  }

  def runGa(input: String, generation: Int, correctSet: ArrayBuffer[Classifier]): Unit = {
    // Parent selection
    val firstParent = gaFirstParentSelection match {
      case TournamentSelection(size) => tournamentSelection(correctSet, size)
      case RouletteWheelSelection => rouletteWheelSelection(correctSet)
    }

    val secondParent = gaSecondParentSelection match {
      case Panmixic => gaFirstParentSelection match {
        case TournamentSelection(size) => tournamentSelection(correctSet -= firstParent, size)
        case RouletteWheelSelection => rouletteWheelSelection(correctSet -= firstParent)
      }
      case PhenotypeInbreeding => getSecondParentPhIn(correctSet, firstParent)
      case PhenotypeOutbreeding => getSecondParentPhOut(correctSet, firstParent)
      case GenotypeInbreeding => getSecondParentGeIn(correctSet, firstParent)
      case GenotypeOutbreeding => getSecondParentGeOut(correctSet, firstParent)
    }

    // Crossover
    var (offspring1, offspring2) = if (Random.nextDouble() < crate) {
      crossover(firstParent, secondParent, generation)
    } else {
      (firstParent.copy(), secondParent.copy())
    }

    population

    // Mutation
    offspring1 = mutation(offspring1, input)
    offspring2 = mutation(offspring2, input)

    population

    // Insert into the population
    insertRule(offspring1)
    insertRule(offspring2)

    // Subsumption
    while (populationSize < population.map(_.numerosity).sum) {
      deleteFromPopulation()
    }
  }

  // 7.1 Selection
  // 7.1.1 Standart (panmixic) selection
  def rouletteWheelSelection(correctSet: ArrayBuffer[Classifier]): Classifier = {
    val totalFitness: Double = population.map(_.fitness).sum
    population.map(c => (c, c.fitness / totalFitness)).maxBy(_._2)._1
  }

  // Preferred approach
  def tournamentSelection(correctSet: ArrayBuffer[Classifier], tournamentSize: Int = 2): Classifier = {
    Random.shuffle(correctSet.indices.toList).take(tournamentSize).map(idx => correctSet(idx)).maxBy(_.fitness)
  }

  def phenotype(correctSet: ArrayBuffer[Classifier], firstParent: Classifier): ArrayBuffer[(Classifier, Double)] = {
    correctSet.filterNot(_ == firstParent).map(c => (c, Math.abs(c.fitness - firstParent.fitness)))
  }

  def genotype(correctSet: ArrayBuffer[Classifier], firstParent: Classifier): ArrayBuffer[(Classifier, Double)] = {
    correctSet.filterNot(_ == firstParent).map { c =>
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

  // 7.1.2 Phenotype inbreeding for second parent selection
  def getSecondParentPhIn(correctSet: ArrayBuffer[Classifier], firstParent: Classifier): Classifier = {
    phenotype(correctSet, firstParent).minBy(_._2)._1
  }

  // 7.1.3 Genotype inbreeding for second parent selection
  def getSecondParentGeIn(correctSet: ArrayBuffer[Classifier], firstParent: Classifier): Classifier = {
    genotype(correctSet, firstParent).maxBy(_._2)._1
  }

  // 7.1.4 Phenotype outbreeding for second parent selection
  def getSecondParentPhOut(correctSet: ArrayBuffer[Classifier], firstParent: Classifier): Classifier = {
    phenotype(correctSet, firstParent).maxBy(_._2)._1
  }

  // 7.1.5 Genotype outbreeding for second parent selection
  def getSecondParentGeOut(correctSet: ArrayBuffer[Classifier], firstParent: Classifier): Classifier = {
    genotype(correctSet, firstParent).minBy(_._2)._1
  }

  // 7.2 Crossover
  def crossover(firstParent: Classifier, secondParent: Classifier, generation: Int): (Classifier, Classifier) = {
    val (offspringCondition1, offspringCondition2) = gaCrossoverType match {
      case OnePointCrossover => onePointCrossover(firstParent, secondParent)
      case NPointCrossover(n) => nPointCrossover(n, firstParent, secondParent)
      case UniformCrossover => uniformCrossover(firstParent, secondParent)
    }

    val prediction = (firstParent.prediction + secondParent.prediction) / crossoverPredictionDecay
    val error = (firstParent.error + secondParent.error) * crossoverErrorDecay
    val fitness = (firstParent.fitness + secondParent.fitness) * crossoverFitnessDecay

    (
      firstParent.copy(
        condition = offspringCondition1,
        birthGeneration = generation,
        prediction = prediction,
        error = error,
        fitness = fitness
      ),
      secondParent.copy(
        condition = offspringCondition2,
        birthGeneration = generation,
        prediction = prediction,
        error = error,
        fitness = fitness
      )
    )
  }

  def onePointCrossover(firstParent: Classifier, secondParent: Classifier): (String, String) = {
    val point = Random.nextInt(firstParent.condition.length)

    (
      firstParent.condition.take(point) + secondParent.condition.drop(point),
      secondParent.condition.take(point) + firstParent.condition.drop(point)
    )
  }

  def nPointCrossover(n: Int, firstParent: Classifier, secondParent: Classifier): (String, String) = {
    require(n < firstParent.condition.length, "N point crossover doesn't allow N to be larger than condition length")

    val points = (Random.shuffle(firstParent.condition.indices.toList.filterNot(i => i == 0 || i == firstParent.condition.length)).take(n) ++ List(0, firstParent.condition.length)).sorted

    var newCondition1 = ""
    var newCondition2 = ""

    0 until (points.size - 1) foreach { i =>
      if (Random.nextDouble() < crossoverProbability) {
        newCondition1 = newCondition1 + firstParent.condition.substring(points(i), points(i + 1))
        newCondition2 = newCondition2 + secondParent.condition.substring(points(i), points(i + 1))
      } else {
        newCondition1 = newCondition1 + secondParent.condition.substring(points(i), points(i + 1))
        newCondition2 = newCondition2 + firstParent.condition.substring(points(i), points(i + 1))
      }
    }

    (newCondition1, newCondition2)
  }

  // Preferable
  def uniformCrossover(firstParent: Classifier, secondParent: Classifier): (String, String) = {
    (
      firstParent.condition.zipWithIndex.map { e =>
        val fCh = e._1
        val sCh = secondParent.condition.charAt(e._2)

        if (fCh == sCh) {
          fCh
        } else {
          if (Random.nextDouble() < crossoverProbability) fCh else sCh
        }
      }.mkString
      ,
      firstParent.condition.zipWithIndex.map { e =>
        val fCh = e._1
        val sCh = secondParent.condition.charAt(e._2)

        if (fCh == sCh) {
          fCh
        } else {
          if (Random.nextDouble() < crossoverProbability) fCh else sCh
        }
      }.mkString
    )
  }

  // 7.3 Mutation
  def mutation(initialRule: Classifier, input: String): Classifier = {
    var newCondition = initialRule.condition.zipWithIndex.map { ch =>
      if (Random.nextDouble() < mutationProbability) {
        if (ch._1 == '#') input.charAt(ch._2) else '#'
      } else {
        ch._1
      }
    }.mkString

    initialRule.copy(condition = newCondition)
  }

  // Utils
  def getPredictions(set: ArrayBuffer[Classifier]): Array[Prediction] = {
    set.groupBy(_.action).map { c =>
      val count = c._2.map(_.fitness).sum
      val sum = c._2.map(_.prediction).sum * count
      val weight = if (count > 0) sum / count else 0.0

      Prediction(c._1, sum, count, weight)
    }.toArray
  }

  def selectAction(predictions: Array[Prediction], explore: Boolean = false): Boolean = {
    if (explore) {
      val keys = predictions.map(_.action)
      keys(Random.nextInt(keys.length))
    } else {
      predictions.maxBy(_.weight).action
    }
  }


  def train(): Unit = {
    val performance: ArrayBuffer[Performance] = ArrayBuffer.empty

    val positiveReward = 100.0
    val negativeReward = 0.0
    val possibleActions = Array(true, false)

    0 until trainSetSize foreach { generation =>
      val explore = generation % explorationRate == 0
      val input = multiplexers.generateRandomMultiplexerSignal
      val correctAction = multiplexers.targetFunction(input)

      val matchSet = getMatchSet(input, possibleActions, generation)
      val prediction = getPredictions(matchSet)
      val selectedAction = selectAction(prediction)

      if (explore) {
        //val (set1, set2) = matchSet.groupBy(_.action)

        val grouped = matchSet.groupBy(_.action)

        val (correct, incorrect) = if (selectedAction == correctAction) {
          (grouped.getOrElse(selectedAction, ArrayBuffer.empty), grouped.getOrElse(!selectedAction, ArrayBuffer.empty))
        } else {
          (grouped.getOrElse(!selectedAction, ArrayBuffer.empty), grouped.getOrElse(selectedAction, ArrayBuffer.empty))
        }

        updateSet(correct, positiveReward)
        updateSet(incorrect, negativeReward)

        if (canRunGa(generation, matchSet)) {
          runGa(input, generation, matchSet)
        }
      } /*else {
        performance.append(Performance(
          Math.abs
        ))
      }*/
    }
  }

  def test(): Double = {
    val correct = (0 until testSetSize).map { instance =>
      val input = multiplexers.generateRandomMultiplexerSignal
      val matchSet = population.filter(rule => doesMatch(input, rule.condition))

      if (matchSet.isEmpty) {
        0
      } else {
        val prediction = getPredictions(matchSet)
        val action = selectAction(prediction)
        if (multiplexers.targetFunction(input) == action) 1 else 0
      }
    }.sum

    //println(s"Done! Classified correctly ${(correct.toDouble / testSetSize.toDouble).asPercentage}")

    correct.toDouble / testSetSize.toDouble
  }

  def run(): Unit = {
    train()

    println(s"Population consists of: \n${population.map(_.toString).mkString("\n")}\n")

    println(s"Average accuracy is: ${
      ((0 until runTimes).map { i =>
        test()
      }.sum / runTimes.toDouble).asPercentage
    }")
  }

  implicit class DoubleAsPercentage(d: Double) {
    def asPercentage: String = NumberFormat.getPercentInstance.format(d)
  }

}

object Test extends App {
  XCS().run()
}