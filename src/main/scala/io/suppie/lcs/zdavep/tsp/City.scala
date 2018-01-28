package io.suppie.lcs.zdavep.tsp

import io.suppie.lcs.zdavep.genetic.Genetic.Gene

/**
  * Defines a city gene as a name, latitude and longitude.
  */
case class City(name: String, lat: Double, lon: Double) extends Gene {
  override def toString: String = name

  override def copy: City = City(name, lat, lon)
}
