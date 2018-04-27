package me.suppie.xcs.generic.contracts

import me.suppie.xcs.generic.Classifier

import scala.util.Random

/**
  * This trait describes classifier matching logic with environment input state as well as covering mechanism if no
  * match was found
  *
  * @tparam A is the type of the action in current environment
  */
trait CoveringMechanism[A] {
  /**
    * Matching logic for classifiers to given input binary [[String]]
    *
    * @param input      is the binary [[String]] representation of the current environment state
    * @param classifier is a member of current classifier' population in algorithm
    * @return true, if classifier matches the input string
    */
  def matchCriteria(input: String, classifier: Classifier[A]): Boolean

  /**
    * Generates new classifier for given input (covering)
    *
    * @param input      is the binary [[String]] representation of the current environment state
    * @param actions    is the list of available actions in current environment
    * @param generation is the number of generation new classifier was generated on
    * @param rng        is the [[Random]] instance, which can be used during classifier generation
    * @return new classifier instance
    */
  def cover(input: String, actions: Array[A], generation: Double, rng: Random): Classifier[A]
}
