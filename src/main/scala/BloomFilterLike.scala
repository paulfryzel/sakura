package sakura

trait BloomFilterLike {
  def add(element: String): Unit
  def query(element: String): Boolean
}
