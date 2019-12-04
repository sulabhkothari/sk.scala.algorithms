package DynamicProgramming

import scala.collection.mutable

object VowelsAndConsonants extends App {
  val map = mutable.Map[(BigInt, BigInt), BigInt]()
  map((1, 1)) = 26
  map((1, 0)) = 21

  val consonants = 21
  val vowels = 5
  val alphabets = 26


  //  for {
  //    x <- 1 to l
  //    y <- 0 to (if (x < l) x else m)
  //  } {
  //    f(x, y)
  //  }

  //println(map(l,m))
  //println(map(3,1))
  //println(rf(l, m))
  //println(f(3,1))
  println(calculatePermutations(3, 2))

  def f(length: Int, maxVowels: Int): BigInt = {
    if (length <= 0) return 0
    println(s"$length,$maxVowels")
    if (map.contains((length, maxVowels))) return map((length, maxVowels))

    if (length == maxVowels) {
      map((length, maxVowels)) = BigInt(alphabets).pow(length)
      return map((length, maxVowels))
    }

    if (maxVowels == 0) {
      map((length, maxVowels)) = BigInt(consonants).pow(length)
      return map((length, maxVowels))
    }

    map((length, maxVowels)) = 21 * f(length - 1, maxVowels) + 5 * f(length - 1, maxVowels - 1)
    map((length, maxVowels))
  }

  def calculatePermutations(length: Int, maxVowels: Int): BigInt = {

    def recCalculate(lengthOfSubstring: Int, vowelsInSubstring: Int): BigInt = {
      println(s"$lengthOfSubstring,$vowelsInSubstring")
      if(lengthOfSubstring == 1 && vowelsInSubstring == 0) 21
      else if(lengthOfSubstring == 1 && vowelsInSubstring > 0) 26
      else if (lengthOfSubstring <= 0 ) 0
      else if (lengthOfSubstring == vowelsInSubstring) BigInt(alphabets).pow(lengthOfSubstring)
      else if (vowelsInSubstring == 0) {
          21 * recCalculate(lengthOfSubstring - 1, maxVowels)
      }
      else BigInt(21) * recCalculate(lengthOfSubstring - 1, maxVowels) + BigInt(5)* recCalculate(lengthOfSubstring - 1, vowelsInSubstring - 1)
    }

    recCalculate(length, maxVowels)
  }
}

// (3,1) = 18081
// (4,1) = 412,776
// (5,3) = 11812626
// (2,2) = 21 * (1,2) + 5 * (1,1) = 0 + 26 * 5 = 130
// 21 * 21 + 21 * 5 + 5*5
