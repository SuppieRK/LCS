package io.suppie.lcs.ga

import io.suppie.lcs.ga.Chromosome.Pool

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

case class GeneticIndices(chOpIdx: Int, geneOpIdx: Int) {
  override def toString: String = s"ch index: $chOpIdx gene index: $geneOpIdx"
}

class Population[T <: Gene](limit: Int, val chromosomes: Pool[T]) {

  import Population._

  require(chromosomes.nonEmpty, "Population.check: Undefined initial set of chromosomes")
  require(chromosomes.nonEmpty && chromosomes.size < limit, s"Population.check: The pool of chromosomes ${chromosomes.size} is out of range")
  require(limit > 1 && limit < MaxAmountOfChromosomes, s"Maximum number of allowed chromosomes $limit is out of range")

  @volatile private var marker: Double = 0.0

  @inline final def averageCost: Double = marker

  @inline final def size: Int = chromosomes.size

  @inline final def isEmpty: Boolean = chromosomes.isEmpty

  final def geneSize: Int = Try(chromosomes.head.code.head.size).getOrElse(-1)

  final def chromosomeSize: Int = Try(chromosomes.head.size).getOrElse(-1)

  final def fittest: Option[Chromosome[T]] = if (size > 0) Some(chromosomes.head) else None

  def +(that: Population[T]): Population[T] = {
    require(!that.isEmpty, "Population + : Cannot add an undefined list of chromosomes")
    if (that.size > 0) Population[T](limit, chromosomes ++: that.chromosomes) else this
  }

  def select(score: Chromosome[T] => Unit, cutOff: Double): Unit = {
    require(cutOff > 0.0 && cutOff < 1.01, s"Population.select Cannot select with a cutoff $cutOff out of range")

    // Compute the cumulative score for the entire population
    val cumul = chromosomes.map(_.cost).sum / ScalingFactor
    marker = cumul / chromosomes.size

    // Normalize each chromosome unfitness value
    chromosomes foreach (_.normalizeFitness(cumul))

    // Sorts the chromosome by the increasing value of their unfitness
    val newChromosomes = chromosomes.sortWith(_.cost < _.cost)


    // Apply a cutoff value to the current size of the population
    // if the cutoff has been defined.
    val cutOffSize: Int = (cutOff * newChromosomes.size).floor.toInt
    val newPopSize = if (limit < cutOffSize) limit else cutOffSize

    chromosomes.clear()
    chromosomes ++= newChromosomes.take(newPopSize)
  }

  def crossover(xOver: Double): Unit = {
    require(xOver > 0.0 && xOver < 1.0, s"Population cross-over factor $xOver on the population is out of range")

    // It makes sense to cross over all the chromosomes in this
    // population if there are more than one chromosome
    if (size > 1) {
      // Breakdown the sorted list of chromosomes into two segments
      val mid = size >> 1
      val bottom = chromosomes.slice(mid, size)

      // Pair a chromosome for one segment with a chromosome from the other segment.
      // Then add those offsprings to the current population
      val gIdx = geneticIndices(xOver)
      val offSprings = chromosomes.take(mid)
        .zip(bottom)
        .map { case (t, b) => t.crossover(b, gIdx) }
        .unzip
      chromosomes ++= offSprings._1 ++ offSprings._2
    }
  }

  def mutation(mu: Double): Unit = {
    require(mu > 0.0 && mu < 1.0, s"Population mutation factor $mu on the population is out of range")
    chromosomes ++= chromosomes.map(_.mutation(geneticIndices(mu)))
  }

  def fittest(depth: Int): Option[Pool[T]] = {
    require(depth > 0, s"Population.fittest Incorrect number of chromosomes: $depth should be >0")

    if (size > 1) Some(chromosomes.take(if (depth > size) size else depth)) else None
  }

  def diff(that: Population[T], depth: Int): Option[Pool[T]] = {
    require(that.size > 1, "Population.diff Other population has no chromosome")
    require(depth > 0, s"Population.diff depth $depth should be >1")

    // Define the number of chromosomes participating
    // to the comparison of two populations 'this' and 'that'
    val fittestPoolSize = {
      if (depth >= size || depth >= that.size)
        if (size < that.size) size else that.size
      depth
    }
    // Deals with nested options. Get the 'depth' most fit
    // chromosomes for this population and 'depth' most fit
    // chromosomes for that population, then compare..
    for {
      first <- fittest(fittestPoolSize)
      second <- that.fittest(fittestPoolSize)
      if !first.zip(second).exists { case (x1, x2) => x1 != x2 }
    } yield first
  }

  override def toString: String = chromosomes.map(_.toString).mkString("\n")

  final def symbolic(): String = chromosomes.map(_.symbolic).mkString("\n")

  protected def +=(newCode: List[T]): Unit = {
    require(newCode.nonEmpty, "Population.+=: Cannot add an undefined chromosome")
    require(newCode.lengthCompare(chromosomes.head.size) == 0, s"Population.+=: Number of genes ${newCode.size} != chromosome size ${chromosomes.head.size}")
    chromosomes += new Chromosome[T](newCode)
  }

  private[this] def geneticIndices(prob: Double): GeneticIndices = {
    var idx = (prob * chromosomeSize).floor.toInt
    val chIdx = if (idx == chromosomeSize) chromosomeSize - 1 else idx

    idx = (prob * geneSize).floor.toInt

    val gIdx = if (idx == geneSize) geneSize - 1 else idx
    GeneticIndices(chIdx, gIdx)
  }
}

object Population {
  private val ScalingFactor: Int = 100
  private val MaxAmountOfChromosomes: Int = 10000

  def apply[T <: Gene](limit: Int, chromosomes: Pool[T]): Population[T] = new Population[T](limit, chromosomes)

  def apply[T <: Gene](limit: Int, chromosomes: List[Chromosome[T]]): Population[T] = new Population[T](limit, new Pool[T] ++ chromosomes)

  def empty[T <: Gene]: Population[T] = new Population[T](-1, ArrayBuffer.empty)
}
