package io.suppie.lcs.another

object Entities {

  case class Performance(
                          error: Double,
                          correct: Double
                        )

  case class Prediction(
                         action: Boolean,
                         sum: Double,
                         count: Double,
                         weight: Double
                       )

  case class Classifier(
                         condition: String,
                         action: Boolean,
                         generation: Double,
                         var prediction: Double = 10.0,
                         var error: Double = 0.0,
                         var fitness: Double = 10.0,
                         var experience: Double = 0.0,
                         var setSize: Double = 1.0,
                         var numerosity: Double = 1.0
                       )

}
