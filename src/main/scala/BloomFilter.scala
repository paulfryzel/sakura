package sakura

import scala.collection.mutable.BitSet
import scala.math.abs
import scala.util.MurmurHash3

/*
 * A simple BloomFilter implementation
 * see: http://en.wikipedia.org/wiki/Bloom_filter
 */
object BloomFilter {

  /*
   * Constructs a BloomFilter
   *
   * @param m number of elements
   * @param k number of buckets per element
   * @return a new BloomFilter
   */
  def getFilter(m: Int, k: Int = 5) = {
    new BloomFilter(m, k)
  }

  /*
   * Constructs an empty BloomFilter
   *
   * @return a new empty BloomFilter
   */
  def emptyFilter() = {
    new BloomFilter(0, 0)
  }

  private val EXCESS = 20
}

class BloomFilter(m: Int, k: Int) extends BloomFilterLike {
  private[this] val capacity = m * k + BloomFilter.EXCESS
  private[this] val buckets = new BitSet(capacity)
  private[this] val hasher = new MurmurHash3

  override def add(element: String) {
    hash(element).foreach(buckets += _)
  }

  override def query(element: String): Boolean = {
    for (i <- hash(element)) {
      if (!buckets.contains(i)) return false
    }
    true
  }

  /*
   * Hash element into k-buckets using two hash functions
   * see: https://www.eecs.harvard.edu/~michaelm/postscripts/tr-02-05.pdf
   */
  private[this] def hash(element: String): Array[Int] = {
    val h1 = hasher.stringHash(element, 0)
    val h2 = hasher.stringHash(element, h1)
    (for (i <- 0 until k) yield abs((h1 + i * h2) % capacity)).toArray
  }

  def isEmpty() = buckets.isEmpty

  def size() = buckets.size

  override def toString = {
    if(buckets.isEmpty) {
      "BloomFilter()"
    } else {
      buckets.tail.foldLeft("BloomFilter(" + buckets.head)(_ + ", " + _) + ")"
    }
  }
}
