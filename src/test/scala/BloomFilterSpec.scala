import org.scalatest._

class BloomFilterSpec extends FlatSpec with Matchers {

  "BloomFilter" should "contain item" in {
    val bloom = new BloomFilter(4, 1000, 4000)
    bloom.add("dave")
    bloom.add("ace")
    bloom.add("daver")

    assert(bloom.contains("dave") === true)
    assert(bloom.contains("ace") === true)
    assert(bloom.contains("daver") === true)

    assert(bloom.contains("evad") === false)
    assert(bloom.contains("eca") === false)
    assert(bloom.contains("revad") === false)
  }

  "BloomFilter" should "not contain item" in {
    val bloom = new BloomFilter(4, 1000, 4000)
    assert(bloom.contains("dave") === false)
  }

  "BloomFilter" should "add dictionary entries" in {
    val wordCount = Dictionary.wordCount
    val k = 2
    val bloom = BloomFilterWrapper(wordCount, 0.1)
    Dictionary.processEntries(bloom.add)

    assert(bloom.contains("car") === true)
    assert(bloom.contains("zzrtyfasfd") === false)
    assert(bloom.contains("zzr") === false)
    assert(bloom.additions === wordCount)
  }

  "BloomFilter" should "report collisions" in {
    val wordCount = Dictionary.wordCount
    BloomFilterWrapper(wordCount, 0.1)

    filterDictionary(BloomFilterWrapper(wordCount, 0.1))
    filterDictionary(BloomFilterWrapper(wordCount, 0.01))
    filterDictionary(BloomFilterWrapper(1, wordCount, 400000))
    filterDictionary(BloomFilterWrapper(2, wordCount, 400000))
    filterDictionary(BloomFilterWrapper(4, wordCount, 400000))
    filterDictionary(BloomFilterWrapper(1, wordCount, 200000))
    filterDictionary(BloomFilterWrapper(2, wordCount, 200000))
    filterDictionary(BloomFilterWrapper(2, wordCount, 4000000))
  }

  def filterDictionary(wrapper: BloomFilterWrapper.BloomFilterWrapper) = {
    Dictionary.processEntries(wrapper.add)
    println(wrapper.results)
  }

  private object BloomFilterWrapper {
    def apply(k: Int, m: Int, n: Int) = {
      new BloomFilterWrapper(BloomFilter(k, m, n))
    }

    def apply(n: Int, p: Double) = {
      new BloomFilterWrapper(BloomFilter(n, p))
    }

    class BloomFilterWrapper(bloomFilter: BloomFilter) {
      private var collisions, additions_ = 0

      def numberOfCollisions = collisions

      def additions = additions_

      def add(value: String): Unit = {
        additions_ += 1
        if (contains(value)) collisions += 1 else bloomFilter.add(value)
      }

      def contains(value: String) = bloomFilter.contains(value)

      def results = s"------\n${bloomFilter.toString} collisions=$collisions\n------\n"
    }

  }

}
