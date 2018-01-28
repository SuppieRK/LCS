package io.suppie.lcs.zdavep.tsp

import io.suppie.lcs.zdavep.genetic

/**
 * Traveling salesman problem example.
 */
object Main extends App {

  // Read a tsp file from CLI. Use a default file if not provided.
  implicit val tspFileReader = new TspFileReader {
    val tspFile = if (args.length >= 1) args(0).trim else "cities.tsp"
    override def readLines: List[String] = scala.io.Source.fromFile(tspFile).getLines().toList
  }

  // Initialize a population
  val offspring = 100
  val size = 1000
  val initialSize = size * 2
  val maxGenerations = 2500
  val pop = genetic.init(initialSize, size)

  // Tail-recursive evolution function
  @scala.annotation.tailrec
  def loop(generation: Int = 1, bestFitness: Double = Double.MaxValue): Unit = if (generation <= maxGenerations) {
    (1 to offspring).foreach { _ => genetic.evolve(pop) }
    val currentBest = pop.min
    val currentFitness = tspFitness.fitness(currentBest)
    if (currentFitness < bestFitness) {
      print(s"Best fitness found = $currentFitness at generation $generation\n")
      print(currentBest.mkString(" -> ") + "\n\n")
      loop(generation + 1, currentFitness)
    } else {
      loop(generation + 1, bestFitness)
    }
  }

  // Start evolving
  loop()
}
