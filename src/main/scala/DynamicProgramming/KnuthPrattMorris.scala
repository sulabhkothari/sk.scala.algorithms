package DynamicProgramming

object KnuthPattMorris extends App {
  def kmpMatcher(t: String, p: String) = {
    val n = t.length
    val m = p.length
    val pi = computePrefixFunction(p)
    var q = 0
    for (i <- 0 until n) {
      while (q > 0 && p(q) != t(i)) q = pi(q - 1)
      if (p(q) == t(i)) q = q + 1
      if (q == m) {
        println(s"Pattern occurs with shift ${i - m + 2}")
        q = pi(q - 1)
      }
    }
  }

  def computePrefixFunction(p: String) = {
    val m = p.length
    val pi: Array[Int] = new Array(m)
    pi(0) = 0
    var k = 0
    for (q <- 2 to m) {
      while (k > 0 && p(k) != p(q - 1)) k = pi(k - 1)
      if (p(k) == p(q - 1)) k = k + 1
      pi(q - 1) = k
    }
    println(pi.mkString(","))
    pi
  }

  kmpMatcher("aaababaabaababaab", "aabab")
  println("===========================")
  kmpMatcher("bacbababacabcbab", "ababaca")
}
