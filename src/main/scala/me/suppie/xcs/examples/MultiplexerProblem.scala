package me.suppie.xcs.examples

import me.suppie.xcs.Xcs

object MultiplexerProblem extends App {
  val xcsInstance = Xcs()

  xcsInstance.trainModel()

  0 until 10 foreach { i =>
    println(s"Testing model: attempt #$i - ${xcsInstance.testModel()}")
  }
}
