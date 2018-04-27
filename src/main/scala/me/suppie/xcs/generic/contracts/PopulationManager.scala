package me.suppie.xcs.generic.contracts

import me.suppie.xcs.generic.Classifier

import scala.collection.mutable.ArrayBuffer

/**
  * Pluggable rule population management to ease the research and extension capability
  *
  * @tparam A is the type of the actions, available for current environment state
  */
trait PopulationManager[A] {
  // Shorthand type
  type Population = ArrayBuffer[Classifier[A]]

  /**
    * Insert rule into population
    *
    * @param population link
    * @param classifier to be inserted
    */
  def insert(population: Population, classifier: Classifier[A]): Unit

  /**
    * Update classifier parameters in the population
    *
    * @param classifiers retrieved from population and used to form a match set during action prediction stage
    * @param reward      received from taking predicted action in the environment
    */
  def updateClassifiers(classifiers: Population, reward: Double): Unit

  /**
    * Compact population (sometimes this operation called `delete`, but `compact` fits here more in my opinion)
    *
    * @param population link
    */
  def compact(population: Population): Unit

  /**
    * Shorthand for checking if algorithm should use subsumption mechanism
    *
    * @return true, if we want to use subsumption and provided an implementation, false otherwise
    */
  def subsumptionEnabled: Boolean

  /**
    * Subsume classifiers in population
    *
    * Subsumption is a process where classifiers are subsumed by more general classifiers
    *
    * More general classifiers are searched by the [[Classifier.condition]] value in a following way:
    * If first condition contains more wildcards than the second while other values are the same -
    * then second condition can be safely subsumed by more general first condition, if their accuracy values are the same
    * or first classifier' accuracy is bigger than second classifier' accuracy
    *
    * @param population link
    */
  def subsume(population: Population): Unit
}
