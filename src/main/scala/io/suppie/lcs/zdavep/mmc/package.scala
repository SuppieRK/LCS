package io.suppie.lcs.zdavep

/**
  * MMC-specific genetic algorithm values and functions
  */
package object mmc {

  import genetic._
  import Genetic._

  import scala.util.Random._

  // Create a series of coin combinations
  private val pool = for {
    q <- 0 to Quarter.max
    d <- 0 to Dime.max
    n <- 0 to Nickel.max
    p <- 0 to Penny.max
  } yield List(
    Change(q, Quarter), Change(d, Dime), Change(n, Nickel), Change(p, Penny)
  )  // Round a decimal
  def round(v: Double): Double = BigDecimal(v).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble



  // Choose a random integer within a given range
  private def randInt(h: Int, l: Int = 0) = math.floor(nextDouble * (h - l)).toInt + l

  // Choose a change amount at random
  implicit val mmcGenotype: Genotype[Change] = new Genotype[Change] {
    def random: Chromosome[Change] = pool(randInt(pool.length))
  }

  // Determine the fitness of an amount of change. Score is penalized if the exact target change value is not found.
  implicit def mmcFitness(implicit amt: ChangeAmount): Fitness[Change] = new Fitness[Change] {
    def fitness(c: Chromosome[Change]): Double = {
      val value: Double = round(c.foldLeft(0D)(_ + _.value))
      if (value != amt.value) 100D else c.foldLeft(0)(_ + _.n).toDouble
    }
  }

  // Define crossover to just generate one new change amount at random.
  implicit val mmcXover: Xover[Change] = new Xover[Change] {
    def crossover(p: Array[Chromosome[Change]]): Array[Chromosome[Change]] = Array(mmcGenotype.random)
  }

  // No mutation, just return the original amount of change
  implicit val mmcMutate: Mutate[Change] = new Mutate[Change] {
    override def mutate(c: Chromosome[Change]): Chromosome[Change] = c
  }

  // Select two adjacent chromosomes at random
  implicit val mmcSelect: Select[Change] = new Select[Change] {
    def select(pop: Array[Chromosome[Change]]): Array[Chromosome[Change]] = {
      val i = randInt(pop.length - 1)
      Array(pop(i), pop(i + 1))
    }
  }

  // Insertion operation
  implicit def mmcInsert(implicit f: Fitness[Change]): Insert[Change] = new Insert[Change] {
    def insert(c: Chromosome[Change], pop: Array[Chromosome[Change]]): Unit = pop(randInt(pop.length)) = c
  }

  // Minimum fitness is best
  implicit def mmcOrdering(implicit f: Fitness[Change]): Ordering[Chromosome[Change]] =
    Ordering.by((c: Chromosome[Change]) => f.fitness(c))
}
