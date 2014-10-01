import scala.collection.mutable
import scala.util.hashing.MurmurHash3

class BloomFilter(k: Int, n: Int, m: Int) {
  private val bitSet = mutable.BitSet(m)

  def add(value: String) {
    createHashes(value.toCharArray)
      .foreach(bitSet +=)
  }

  def contains(value: String): Boolean = {
    createHashes(value.toCharArray)
      .forall(bitSet.contains)
  }

  def falsePositives = Math.round(Math.pow(1 - Math.pow(1 - 1 / m, n * m), k)).toInt


  private def createHashes(chars: Array[Char]): IndexedSeq[Int] = {
    0.to(k).map(x => Math.abs(MurmurHash3.arrayHash(chars, x)) % m)
  }

  override def toString = s"n=$n, m=$m, k=$k"
}

object BloomFilter {
  def apply(k: Int, n: Int, m: Int) = new BloomFilter(k, n, m)


  def apply(n: Int, p: Double): BloomFilter = {
    val m = calculateBitSetSize(p, n)
    val k = optimumNumberOfHashes(m, n)
    apply(k, n, m)
  }

  private def optimumNumberOfHashes(m: Double, n: Double) = Math.round(m / n * Math.log(2)).toInt

  //p must be (0 < p < 1)
  private def calculateBitSetSize(p: Double, n: Int) = Math.ceil(-n * Math.log(p) / Math.pow(Math.log(2), 2)).toInt

}