package io.suppie.lcs.ga

case class Quantization(toInt: Double => Int, toDouble: Int => Double) {
  def this(R: Int) = this((x: Double) => (x * R).floor.toInt, (n: Int) => n / R)
}

case class Encoding(nValueBits: Int, nOperationBits: Int) {
  val length: Int = nValueBits + nOperationBits
  val valueRange: Range = Range(0, nValueBits)
  val operatorRange: Range = Range(nValueBits, length)
}