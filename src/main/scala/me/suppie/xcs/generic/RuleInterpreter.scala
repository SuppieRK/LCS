package me.suppie.xcs.generic

trait RuleInterpreter[T] {
  def encode(state: T): String

  def decode(state: String): T
}
