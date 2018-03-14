package io.suppie.lcs

object GeneticEntities {

  trait Selection

  trait SecondParentSelection

  trait CrossoverType

  case class TournamentSelection(size: Int = 2) extends Selection

  case class NPointCrossover(n: Int = 2) extends CrossoverType

  case object RouletteWheelSelection extends Selection

  case object Panmixic extends SecondParentSelection

  case object PhenotypeInbreeding extends SecondParentSelection

  case object PhenotypeOutbreeding extends SecondParentSelection

  case object GenotypeInbreeding extends SecondParentSelection

  case object GenotypeOutbreeding extends SecondParentSelection

  case object OnePointCrossover extends CrossoverType

  case object UniformCrossover extends CrossoverType

}
