package me.suppie.xcs.generic.contracts

/**
  * Interpretation mechanism to convert rules to binary form, if needed
  *
  * @tparam T is the type of the environment state representative
  */
trait RuleInterpreter[T] {
  /**
    * State encoding to binary format
    * NOTE: [[LcsEnvironment]] will check if the result is a valid binary string
    *
    * @param state is the instance of the environment state representative
    * @return state' binary [[String]] representation
    */
  def encode(state: T): String

  /**
    * State decoding from binary format
    * NOTE: [[LcsEnvironment]] will check that the input is a valid binary string
    *
    * @param state is the binary [[String]] representation of the state
    * @return the instance of the environment state representative
    */
  def decode(state: String): T
}
