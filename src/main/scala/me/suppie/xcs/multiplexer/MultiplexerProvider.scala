package me.suppie.xcs.multiplexer

import java.util.concurrent.ThreadLocalRandom

/**
  * The provider for multiplexers
  *
  * @param multiplexerOrder The order of the multiplexer
  */
case class MultiplexerProvider(multiplexerOrder: Int = 4) extends IntAdditionalLogic {
  /**
    * Generates random signal for multiplexer
    *
    * @return The signal represented by string
    */
  def generateRandomMultiplexerSignal: String = {
    require(multiplexerOrder.isPowerOfTwo, "Multiplexer order must be the power of two")
    val requiredInputLength = multiplexerOrder + multiplexerOrder.exponentOfTwo
    val rng = ThreadLocalRandom.current()
    (0 until requiredInputLength).map(_ => if (rng.nextDouble() > 0.5) 1 else 0).mkString
  }

  /**
    * Multiplexer target function calculation
    *
    * @param input The input multiplexer value represented by binary string
    * @return The result value of the binary multiplexer
    */
  def targetFunction(input: String): Boolean = {
    require(multiplexerOrder.isPowerOfTwo, "Multiplexer order must be the power of two")
    val requiredInputLength = multiplexerOrder + multiplexerOrder.exponentOfTwo
    require(input.length == requiredInputLength, s"Input length for given multiplexer order of $multiplexerOrder must be $requiredInputLength")
    createMultiplexers(multiplexerOrder).zipWithIndex.map(e => multiply(e._1, e._2, input)).reduceLeft(_ | _).toBoolean
  }

  /**
    * Calculated parts of the boolean multiplexer equation
    *
    * @param mux    The current part of the equation
    * @param muxIdx The index of this part
    * @param input  The input signal
    * @return The result for this equation
    */
  private def multiply(mux: String, muxIdx: Int, input: String): Int = (0 until mux.length).map { i =>
    if (i < mux.length - 1) {
      if (mux(i) == '0') not(input(i)).toBoolInt else input(i).toBoolInt
    } else {
      input(i + muxIdx).toBoolInt
    }
  }.reduceLeft(_ & _)

  private def not(ch: Char): Char = if (ch == '0') '1' else '0'

  /**
    * Creation of the multiplexer arguments
    *
    * @param i The multiplexer order
    * @return The multiplexers
    */
  private def createMultiplexers(i: Int): Array[String] = {
    val stringLength = i.exponentOfTwo
    val threshold = Math.pow(2, stringLength - 1)

    (0 until i).map { n =>
      val str = n.toBinaryString

      if (n < threshold) {
        ("0" * (stringLength - str.length) + str) + "1"
      } else {
        str + "1"
      }
    }.toArray
  }

  implicit class CharToBooleanIntLogic(c: Char) {
    def toBoolInt: Int = if (c == '0') 0 else 1
  }

}
