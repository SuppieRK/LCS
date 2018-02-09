package io.suppie.lcs

object Test extends App {
  val mp = MultiplexerProvider(128)
  val repeatTimes: Int = 10

  0 until repeatTimes foreach { i =>
    val input = mp.generateRandomMultiplexerSignal
    println(s"The signal is $input and the result is ${mp.targetFunction(input)}")
  }
}
