package io.suppie.lcs.zdavep.tsp

/**
 * Generic tsp file reader.
 */
trait TspFileReader {
  def readLines: List[String]
}
