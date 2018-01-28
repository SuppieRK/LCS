package io.suppie.lcs.zdavep.mmc

/**
  * Defines a coin.
  */
sealed trait Coin {
  val value: Double
  val max: Int
}

/**
  * Define a $0.25 piece with a max number of 3 coins.
  */
case object Quarter extends Coin {
  val value = 0.25D
  val max = 3
}

/**
  * Define a $0.10 piece with a max number of 2 coins.
  */
case object Dime extends Coin {
  val value = 0.10D
  val max = 2
}

/**
  * Define a $0.05 piece with a max number of 1 coin.
  */
case object Nickel extends Coin {
  val value = 0.05D
  val max = 1
}

/**
  * Define a $0.01 piece with a max number of 4 coins.
  */
case object Penny extends Coin {
  val value = 0.01D
  val max = 4
}
