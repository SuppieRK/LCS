package io.suppie.lcs

object GeneticEntities {

  trait Selection

  case class TournamentSelection(size: Int = 2) extends Selection

  case object RouletteWheelSelection extends Selection

  trait SecondParentSelection

  case object Panmixic extends SecondParentSelection

  case object PhenotypeInbreeding extends SecondParentSelection

  case object PhenotypeOutbreeding extends SecondParentSelection

  case object GenotypeInbreeding extends SecondParentSelection

  case object GenotypeOutbreeding extends SecondParentSelection

  trait CrossoverType

  case object OnePointCrossover extends CrossoverType

  case class NPointCrossover(n: Int = 2) extends CrossoverType

  case object UniformCrossover extends CrossoverType

}
