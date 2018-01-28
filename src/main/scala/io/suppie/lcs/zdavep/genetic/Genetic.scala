package io.suppie.lcs.zdavep.genetic

/**
  * Top-level genetic algorithm types
  */
object Genetic {

  /**
    * A potential solution (ordered list of Genes).
    */
  type Chromosome[T <: Gene] = List[T]

  /**
    * The basic unit of a genetic algorithm.
    */
  trait Gene {
    self =>
    def copy: Gene = self
  }

  /**
    * Fitness function
    */
  trait Fitness[T <: Gene] {
    def fitness(c: Chromosome[T]): Double
  }

  /**
    * Search space
    */
  trait Genotype[T <: Gene] {
    def random: Chromosome[T]
  }

  /**
    * Mutate operation
    */
  trait Mutate[T <: Gene] {
    def mutate(c: Chromosome[T]): Chromosome[T]
  }

  /**
    * Select operation
    */
  trait Select[T <: Gene] {
    def select(pop: Array[Chromosome[T]]): Array[Chromosome[T]]
  }

  /**
    * Crossover operation
    */
  trait Xover[T <: Gene] {
    def crossover(c: Array[Chromosome[T]]): Array[Chromosome[T]]
  }

  /**
    * Insert operation
    */
  trait Insert[T <: Gene] {
    def insert(c: Chromosome[T], p: Array[Chromosome[T]]): Unit
  }

}
