package io.suppie.lcs.zdavep

/**
 * TSP-specific genetic algorithm values and functions
 */
package object tsp {
  import genetic._, Genetic._
  import scala.annotation.tailrec
  import scala.util.Random.nextDouble

  // The radius of earth in miles
  private final val RADIUS_EARTH = 3958.761

  // GCD calculation helper constant
  private final val PI_RADS_PER_180_DEG = math.Pi / 180D

  // TSP crossover rate
  private final val XOVER_RATE = 0.9D

  // TSP mutation rate
  private final val MUTATE_RATE = 0.1D

  // Choose a random integer within a given range
  private def randInt(h: Int, l: Int = 0) = math.floor(nextDouble * (h - l)).toInt + l

  // Great circle distance function (assumes earth is a sphere)
  def distance(c1: City, c2: City): Double = {
    val p1 = c1.lat * PI_RADS_PER_180_DEG
    val p2 = c2.lat * PI_RADS_PER_180_DEG
    val p3 = c2.lon * PI_RADS_PER_180_DEG - c1.lon * PI_RADS_PER_180_DEG
    val p4 = math.sin(p1) * math.sin(p2)
    val p5 = math.cos(p1) * math.cos(p2) * math.cos(p3)
    RADIUS_EARTH * math.acos(p4 + p5)
  }

  // Given a city, find it's nearest neighbor
  private def findNearest(city: City, list: List[City]): City =
    list.map(c => (distance(city, c), c)).minBy(_._1)._2

  // Nearest neighbor optimization function
  @tailrec
  private def nearestNeighbor(c: City, src: List[City], dest: List[City] = Nil): Chromosome[City] =
    if (src.isEmpty) dest :+ c else {
      val nearest = findNearest(c, src)
      nearestNeighbor(nearest, src.filterNot(_ == nearest), dest :+ c)
    }

  // TSP genotype - represents the search space of possible tours (lists of cities)
  implicit def tspGenotype(implicit r: TspFileReader): Genotype[City] = new Genotype[City] {
    import scala.util.Random.shuffle

    private val genePool = r.readLines.map { line =>
      val Array(name, lat, lon) = line.split("\\s")
      City(name.trim, lat.toDouble, lon.toDouble)
    }

    def random: Chromosome[City] = shuffle(genePool.map(_.copy)) match {
      case h :: t => nearestNeighbor(h, t)
      case Nil => genePool
    }
  }

  // TSP fitness - uses the great circle distance function to calculate the length of a tour
  implicit val tspFitness: Fitness[City] = new Fitness[City] {
    @tailrec private def calc(cities: List[City], acc: Double): Double = cities match {
      case h :: t => if (t.isEmpty) acc else calc(t, acc + distance(h, t.head))
      case Nil => acc
    }
    def fitness(c: Chromosome[City]): Double = calc(c, distance(c.last, c.head))
  }

  // TSP crossover - split two chromosomes at a single index and cross combine
  implicit val tspXover: Xover[City] = new Xover[City] {
    def crossover(p: Array[Chromosome[City]]): Array[Chromosome[City]] =
      if (nextDouble > XOVER_RATE) Array.empty[Chromosome[City]] else {
        val (p0, p1) = (p(0), p(1))
        val len = p0.length
        val c0 = p0.splitAt(randInt(len * 2 / 3, len / 3))._1
        val c1 = p1 diff c0
        Array(c0 ++ c1, c1 ++ c0)
      }
  }

  // TSP mutation - reverse an internal slice of a tour
  implicit val tspMutate: Mutate[City] = new Mutate[City] {
    def mutate(c: Chromosome[City]): Chromosome[City] = if (nextDouble > MUTATE_RATE) c else {
      val l1 = c.length
      val l2 = l1 / 2
      val (i1, i2) = (randInt(l2), randInt(l1, l2))
      c.take(i1) ++ c.slice(i1, i2).reverse ++ c.drop(i2)
    }
  }

  // TSP selection - select two chromosomes at random
  implicit val tspSelect: Select[City] = new Select[City] {
    def select(pop: Array[Chromosome[City]]): Array[Chromosome[City]] = Array(
      pop(randInt(pop.length)), pop(randInt(pop.length))
    )
  }

  // TSP Insertion - Replace random existing solution if child has better fitness.
  implicit def tspInsert(implicit f: Fitness[City]): Insert[City] = new Insert[City] {
    def insert(c: Chromosome[City], pop: Array[Chromosome[City]]): Unit = {
      val i = randInt(pop.length)
      if (f.fitness(c) < f.fitness(pop(i))) {
        pop(i) = c
      }
    }
  }

  // TSP ordering - minimum fitness is best.
  implicit def tspOrdering(implicit f: Fitness[City]): Ordering[Chromosome[City]] =
    Ordering.by((c: Chromosome[City]) => f.fitness(c))
}
