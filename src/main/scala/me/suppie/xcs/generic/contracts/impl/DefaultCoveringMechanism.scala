package me.suppie.xcs.generic.contracts.impl

import me.suppie.xcs.generic.Classifier
import me.suppie.xcs.generic.contracts.CoveringMechanism
import me.suppie.xcs.generic.contracts.LcsConstants._

import scala.util.Random

/**
  * Default covering mechanism, which can be used in a variety of different cases
  *
  * @param wildcardProbability is the probability of wildcard to appear while generating new classifier' condition
  * @tparam A is the type of the action in current environment
  */
case class DefaultCoveringMechanism[A](
                                        wildcardProbability: Double = 0.33
                                      ) extends CoveringMechanism[A] {
  /**
    * Matching logic for classifiers to given input binary [[String]]
    *
    * @param input      is the binary [[String]] representation of the current environment state
    * @param classifier is a member of current classifier' population in algorithm
    * @return true, if classifier matches the input string
    */
  override def matchCriteria(input: String, classifier: Classifier[A]): Boolean = input
    .zipWithIndex
    .forall(c => classifier.condition.charAt(c._2) == BinaryWildCard || classifier.condition.charAt(c._2) == c._1)

  /**
    * Generates new classifier for given input (covering)
    *
    * @param input   is the binary [[String]] representation of the current environment state
    * @param actions is the list of available actions in current environment
    * @param rng     is the [[Random]] instance, which can be used during classifier generation
    * @return new classifier instance
    */
  override def cover(input: String, actions: Array[A], generation: Double, rng: Random): Classifier[A] = Classifier(
    input.map(ch => if (rng.nextDouble() < wildcardProbability) BinaryWildCard else ch).mkString,
    actions(rng.nextInt(actions.length)),
    generation
  )
}
