package io.suppie.lcs.zdavep

package object genetic {

  import Genetic._

  /**
    * Initialize a population (search space of potential solutions).
    */
  def init[T <: Gene](initSize: Int, size: Int)(implicit g: Genotype[T], f: Fitness[T]): Array[Chromosome[T]] =
    (1 to initSize).map(_ => g.random).sortBy(f.fitness).take(size).toArray

  /**
    * Evolve a population for a single generation.
    */
  def evolve[T <: Gene](p: Array[Chromosome[T]])(implicit s: Select[T], f: Fitness[T], x: Xover[T], m: Mutate[T], i: Insert[T]): Unit =
    x.crossover(s.select(p)).map(m.mutate).foreach(i.insert(_, p))
}
