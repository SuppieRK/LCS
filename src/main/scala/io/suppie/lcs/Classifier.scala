package io.suppie.lcs

/**
  *
  * @param condition
  * @param action
  * @param birthIteration
  * @param prediction
  * @param error
  * @param fitness
  * @param exp
  * @param averageMatchSetSize
  * @param numerosity The number of copies that symbolically exist in the population
  */
case class Classifier(
                       var condition: String,
                       var action: Boolean,
                       var birthIteration: Double,
                       var prediction: Double = 10.0,
                       var error: Double = 0.0,
                       var fitness: Double = 10.0,
                       var exp: Double = 0.0,
                       var averageMatchSetSize: Double = 1.0,
                       var numerosity: Double = 1.0
                     )
