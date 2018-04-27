package me.suppie.xcs

/**
  * Here defined common entities, used during work
  */
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

  case class Rule(
                   attributes: String,
                   endpoint: Boolean,
                   generation: Double,
                   var prediction: Double = 10.0,
                   var error: Double = 0.0,
                   var fitness: Double = 10.0,
                   var experience: Double = 0.0,
                   var setSize: Double = 1.0,
                   var numerosity: Double = 1.0
                       ) {
    override def toString: String = s"Classifier(" +
      s"condition=$attributes, " +
      s"action=$endpoint, " +
      s"generation=$generation, " +
      s"prediction=$prediction, " +
      s"error=$error, " +
      s"fitness=$fitness, " +
      s"experience=$experience, " +
      s"setSize=$setSize, " +
      s"numerosity=$numerosity" +
      s")"
  }

}
