package me.suppie.xcs.generic

/**
  * Single LCS classifier representation
  *
  * Classifier represents a rule with condition and related action along with associated parameters
  *
  * @param condition  also known as `features` or `independent variables`, referring to a state of the environment
  * @param action     also known as `class` or `dependent variable`, referring to an action to take in certain state
  * @param generation
  * @param prediction
  * @param error
  * @param fitness
  * @param experience
  * @param setSize
  * @param numerosity is the number of classifier copies that symbolically exists in the population
  * @tparam T is the type of the endpoint
  */
case class Classifier[T](
                          condition: String,
                          action: T,
                          generation: Double,
                          var prediction: Double = 10.0,
                          var error: Double = 0.0,
                          var fitness: Double = 0.0,
                          var experience: Double = 0.0,
                          var setSize: Double = 1.0,
                          var numerosity: Double = 1.0
                        )
