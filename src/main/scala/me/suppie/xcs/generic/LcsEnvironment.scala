package me.suppie.xcs.generic

/**
  * Representation of the external environment
  *
  * @tparam ES is the type of the environment state
  * @tparam A  is the type of the action agent is allowed to take
  */
trait LcsEnvironment[ES, A] {
  private final val BinaryStringPattern = "[01]+"

  /**
    * Rule interpreter for environment state conversion in binary form
    *
    * @return [[RuleInterpreter]] instance
    */
  def ruleInterpreter: RuleInterpreter[ES]

  /**
    * Obtain an [[Array]] with actions, available to the agent
    *
    * @return [[Array]] of actions
    */
  def getAvailableActions: Array[A]

  /**
    * Perform certain action in the environment
    *
    * @param action that agent chose to perform
    * @return environment reinforcement for performing the action
    */
  def doAction(action: A): Double

  /**
    * Obtain current environment state
    *
    * @return current environment state
    */
  def getEnvironmentState: ES

  /**
    * Obtain current environment state encoded as binary string
    * Performs check that encoded state is represented as a non-empty binary string
    *
    * @return [[String]] with binary state representation
    */
  final def getEncodedEnvironmentState: String = {
    val result = ruleInterpreter.encode(getEnvironmentState)

    if (result.isEmpty || !result.matches(BinaryStringPattern)) {
      throw new RuntimeException(s"Invalid encode result: $result")
    } else {
      result
    }
  }

  /**
    * Retrieve back environment state from its binary representation
    * Performs check that string to decode is a non-empty binary string
    *
    * @param state [[String]] with binary state representation
    * @return decoded environment state
    */
  final def getDecodedEnvironmentState(state: String): ES = {
    if (state.isEmpty || !state.matches(BinaryStringPattern)) {
      throw new RuntimeException(s"Invalid value for decoding: $state")
    } else {
      ruleInterpreter.decode(state)
    }
  }
}
