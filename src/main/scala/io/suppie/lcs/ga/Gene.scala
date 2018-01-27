package io.suppie.lcs.ga

import java.{util => ju}

import scala.annotation.implicitNotFound

@implicitNotFound("Gene encoding requires double to integer conversion")
@implicitNotFound("Gene encoding requires quantization")
@throws(classOf[IllegalArgumentException])
class Gene(val id: String, val target: Double, val op: Operator)(implicit quant: Quantization, enc: Encoding) extends Cloneable {
  require(!id.isEmpty, "Cannot create a signal with undefined id")

  import Gene._

  def score: Double = -1.0

  lazy val bits: ju.BitSet = encode(target, op)

  @inline final def size: Int = enc.length

  def encode(value: Double, operator: Operator): ju.BitSet = {
    val result = new ju.BitSet(enc.length)
    // Encode the operator
    enc.operatorRange foreach (i => if (((operator.id >> i) & 0x01) == 0x01) result.set(i))
    // Encode the value using the quantization function
    enc.valueRange foreach (i => if (((quant.toInt(value) >> i) & 0x01) == 0x01) result.set(i))
    // Return the result
    result
  }

  def decode(bitSet: ju.BitSet): (Double, Operator) = (
    quant.toDouble(convert(enc.valueRange, bits)),
    op(convert(enc.operatorRange, bits))
  )

  override def clone(): Gene = {
    Range(0, bits.length)./:(Gene(id, target, op))((enc, n) => {
      if (bits.get(n))
        enc.bits.set(n)
      enc
    })
  }

  def toGene(id: String, target: Double, op: Operator) = new Gene(id, target, op)

  override def toString: String = Range(0, bits.size).map(n => if (bits.get(n)) "1" else "0").mkString("")

  def symbolic: String = s"$id ${op.toString} $target"

  def crossover(that: Gene, indices: GeneticIndices): Gene = {
    val clonedBits = cloneBits(bits)

    Range(indices.geneOpIdx, bits.size()) foreach (n => if (that.bits.get(n)) clonedBits.set(n) else clonedBits.clear(n))

    val valOp = decode(clonedBits)

    new Gene(id, valOp._1, valOp._2)
  }

  def mutation(indices: GeneticIndices): Gene = mutation(indices.geneOpIdx)

  def mutation(idx: Int): Gene = {
    val clonedBits = cloneBits(bits)
    // flip the bit
    clonedBits.flip(idx)
    // Decode or convert the bit set into a symbolic representation for the gene
    val valOp = decode(clonedBits)
    new Gene(id, valOp._1, valOp._2)
  }
}

object Gene {
  val DefaultEncoding: Encoding = Encoding(nValueBits = 32, nOperationBits = 2)

  def apply(id: String, target: Double, op: Operator)(implicit quant: Quantization, enc: Encoding): Gene = {
    new Gene(id, target, op)
  }

  private def cloneBits(bits: ju.BitSet): ju.BitSet = {
    Range(0, bits.length)./:(new ju.BitSet)((enc, n) => {
      if (bits.get(n)) enc.set(n)
      enc
    })
  }

  private def convert(r: Range, bits: ju.BitSet): Int = {
    r./:(0)((v, i) => v + (if (bits.get(i)) 1 << i else 0))
  }
}
