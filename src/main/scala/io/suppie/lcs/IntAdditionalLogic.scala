package io.suppie.lcs

trait IntAdditionalLogic {

  implicit class IntAddLogic(i: Int) {
    /**
      * Simple int to boolean conversion
      *
      * @return The boolean representation
      */
    def toBoolean: Boolean = if (i > 0) true else false

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
      * @return True, if this number is the power of two
      */
    def isPowerOfTwo: Boolean = (i != 0) && ((i & i - 1) == 0)

    /**
      * Method to find the exponent of two of the number
      *
      * @return The exponent of two
      */
    def exponentOfTwo: Int = {
      // Change bits in the number
      // EXAMPLE: Let i be 2^7 -> 128 -> 10000000
      val x = i - 1
      // EXAMPLE: Then x will be 127  -> 01111111

      // Split bits into pairs and write how many ones there were into a new number
      val x1 = x - ((x >> 1) & 0x55555555)
      // EXAMPLE: 0x55555555 -> 01010101 01010101 01010101 01010101 -> 1431655765
      // EXAMPLE: (x >> 1)                -> 63  -> 00111111
      // EXAMPLE: ((x >> 1) & 0x55555555) -> 21  -> 00010101
      // EXAMPLE: x1                      -> 106 -> 01101010

      // Repeat process, but now we split bits in groups of 4
      val x2 = (x1 & 0x33333333) + ((x1 >> 2) & 0x33333333)
      // EXAMPLE: 0x33333333 -> 00110011 00110011 00110011 00110011 -> 858993459
      // EXAMPLE: (x1 & 0x33333333)        -> 34 -> 00100010
      // EXAMPLE: (x1 >> 2)                -> 26 -> 00011010
      // EXAMPLE: ((x1 >> 2) & 0x33333333) -> 18 -> 00010010
      // EXAMPLE: x2                       -> 52 -> 00110100

      // Repeat it several more times
      val x3 = (x2 + (x2 >> 4)) & 0x0F0F0F0F
      // EXAMPLE: 0x0F0F0F0F -> 00001111 00001111 00001111 00001111 -> 252645135
      // EXAMPLE: (x2 >> 4)        -> 3  -> 00000011
      // EXAMPLE: (x2 + (x2 >> 4)) -> 55 -> 00110111
      // EXAMPLE: x3               -> 7  -> 00000111

      val x4 = x3 + (x3 >> 8)
      // EXAMPLE: (x3 >> 8) -> 0 -> 00000000
      // EXAMPLE: x4        -> 7 -> 00000111

      val x5 = x4 + (x4 >> 16)
      // EXAMPLE: (x4 >> 16) -> 0 -> 00000000
      // EXAMPLE: x5         -> 7 -> 00000111

      // Remove excess
      x5 & 0x0000007F
      // EXAMPLE: 0x0000007F -> 01111111 -> 127
      // EXAMPLE: 00000111 -> 7
    }
  }

}
