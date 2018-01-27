package io.suppie.lcs.ga.state

sealed abstract class GaState(val description: String)

case class GaSuccess() extends GaState("Success")

case class GaFailure(override val description: String) extends GaState(description)

case class GaRunning() extends GaState("Running")

case class GaIdle() extends GaState("Idle")

case class GaNoConvergence() extends GaState("No Convergence")