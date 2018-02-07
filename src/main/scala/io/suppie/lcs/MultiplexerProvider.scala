package io.suppie.lcs

import java.util.concurrent.ThreadLocalRandom

object MultiplexerProvider {
  def main(args: Array[String]): Unit = {
    val multiplexerOrder: Int = 32
    val repeatTimes: Int = 100

    0 until repeatTimes foreach { i =>
      val input = generateRandomMultiplexerSignal(multiplexerOrder)
      println(s"The signal is $input and the result is ${targetFunction(input, multiplexerOrder)}")
    }
  }

  /**
    * Generates random signal for multiplexer
    *
    * @param multiplexerOrder The order of the multiplexer
    * @return The signal represented by string
    */
  def generateRandomMultiplexerSignal(multiplexerOrder: Int = 4): String = {
    require(isPowerOfTwo(multiplexerOrder), "Multiplexer order must be the power of two")

    val requiredInputLength = multiplexerOrder + findThePowerOfTwo(multiplexerOrder)

    val result = ThreadLocalRandom.current()
      .nextLong((Math.pow(2, requiredInputLength) - 1).toLong)
      .toBinaryString

    if (result.length < requiredInputLength) {
      "0" * (requiredInputLength - result.length) + result
    } else {
      result
    }
  }

  /**
    * Multiplexer target function calculation
    *
    * @param input            The input multiplexer value represented by binary string
    * @param multiplexerOrder The order of the current multiplexer
    * @return The result value of the binary multiplexer
    */
  def targetFunction(input: String, multiplexerOrder: Int = 4): Boolean = {
    require(isPowerOfTwo(multiplexerOrder), "Multiplexer order must be the power of two")

    val requiredInputLength = multiplexerOrder + findThePowerOfTwo(multiplexerOrder)

    require(input.length == requiredInputLength, s"Input length for given multiplexer order of $multiplexerOrder must be $requiredInputLength")

    createMultiplexers(multiplexerOrder).zipWithIndex.map { e =>
      multiplicate(e._1, e._2, input)
    }.reduceLeft(_ | _).toBoolean
  }

  private def not(ch: Char): Char = if (ch == '0') '1' else '0'

  /**
    * Calculated parts of the boolean multiplexer equation
    *
    * @param mux    The current part of the equation
    * @param muxIdx The index of this part
    * @param input  The input signal
    * @return The result for this equation
    */
  private def multiplicate(mux: String, muxIdx: Int, input: String): Int = {
    (0 until mux.length).map { i =>
      if (i < mux.length - 1) {
        if (mux(i) == '0') {
          not(input(i)).toBoolInt
        } else {
          input(i).toBoolInt
        }
      } else {
        input(i + muxIdx).toBoolInt
      }
    }.reduceLeft(_ & _)
  }

  private def createMultiplexers(i: Int): Array[String] = {
    val stringLength = findThePowerOfTwo(i)
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

  /**
    * Method to find the power of two of the number
    *
    * @param i The number we are searching power for
    * @return THe power of two
    */
  private def findThePowerOfTwo(i: Int): Int = {
    // Change bits in the number
    val x = i - 1
    // Split bits into pairs and write how many ones there were into a new number
    val x1 = x - ((x >> 1) & 0x55555555)
    // Repeat process, but now we split bits in groups of 4
    val x2 = (x1 & 0x33333333) + ((x1 >> 2) & 0x33333333)
    // Repeat it several more times
    val x3 = (x2 + (x2 >> 4)) & 0x0F0F0F0F
    val x4 = x3 + (x3 >> 8)
    val x5 = x4 + (x4 >> 16)
    // Remove excess
    x5 & 0x0000003F
  }

  /**
    * Checks if this number is the power of two
    *
    * Explanation:
    * (x != 0) - we eliminate 0 from the check, despite it IS the power of two
    *
    * Let's assume, than the argument is 4, then we have ((4 & 4 - 1) == 0) -> (4 & 3) == 0
    * Simple & is the bitwise operation, so we have: 100 & 011 -> 000 -> 0 == 0 - PASS
    *
    * Now let's assume we have 5, then 5 & 4 -> 101 & 100 -> 100 -> 4 != 0 - FAIL
    *
    * @param i The number to check
    * @return True, if this number is the power of two
    */
  private def isPowerOfTwo(i: Int): Boolean = {
    (i != 0) && ((i & i - 1) == 0)
  }


  implicit class StringToBooleanLogic(s: String) {
    def toBooleanArray: Array[Boolean] = if (!s.exists(ch => ch != '1' || ch != '0')) {
      s.map(ch => if (ch == '1') true else false).toArray
    } else {
      Array.emptyBooleanArray
    }
  }

  implicit class CharToBooleanIntLogic(c: Char) {
    def toBoolInt: Int = if (c == '0') 0 else 1
  }

  implicit class IntToBooleanLogic(i: Int) {
    def toBoolean: Boolean = if (i > 0) true else false
  }

  implicit class BooleanToIntLogic(b: Boolean) {
    def toNumber: Int = if (b) 1 else 0
  }

}
