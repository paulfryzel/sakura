package sakura

import org.specs2.mutable._

object BloomFilterSpec {
  private val bf = BloomFilter.getFilter(10)
  private val ebf = BloomFilter.emptyFilter
}

class BloomFilterSpec extends Specification {
  import BloomFilterSpec._

  "When Sakura is added, the BloomFilter" should {
    bf.add("Sakura")
    "contain the element Sakura" in {
      bf.query("Sakura") must_== true
    }
    "not contain the element sakura" in {
      bf.query("sakura") must_== false
    }
    "have a size of 5" in {
      bf.size must_== 5
    }
  }

  "The empty BloomFilter" should {
    "be empty" in {
      ebf.isEmpty must_== true
    }
    "have a size of 0" in {
      ebf.size must_== 0
    }
  }
}
