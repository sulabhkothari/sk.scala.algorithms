import scala.io.Source

/**
  * Christy is interning at HackerRank. One day she has to distribute some chocolates to her colleagues. She is biased towards her friends and plans to give them more than the others. One of the program managers hears of this and tells her to make sure everyone gets the same number.
  *
  * To make things difficult, she must equalize the number of chocolates in a series of operations. For each operation, she can give  chocolates to all but one colleague. Everyone who gets chocolate in a round receives the same number of pieces.
  *
  * For example, assume the starting distribution is . She can give  bars to the first two and the distribution will be . On the next round, she gives the same two  bars each, and everyone has the same number: .
  *
  * Given a starting distribution, calculate the minimum number of operations needed so that every colleague has the same number of chocolates.
  */
object Equal extends App {

  val input = Source.fromFile("src/main/resources/longinput.txt")

  val line = input.getLines().toArray

  def findStepsRequired(num: Int): Int = {
    val numberOf5 = num / 5
    val remainingFrom5 = num % 5
    val numberOf2 = remainingFrom5 / 2
    val numberOf1 = remainingFrom5 % 2
    val result = numberOf5 + numberOf2 + numberOf1
    result
  }

  def mod5To1(arr: Array[Int]) = {
    var min = arr.min
    val mod51sum = arr.filter(x => (x - min) % 5 == 4).map(x => findStepsRequired(x + 1 - min)).sum
    val sum = arr.filter(x => (x - min) % 5 != 4).map(_ + 1).map(x => findStepsRequired(x - min)).sum
    mod51sum + sum
  }

  def mod5To2(arr: Array[Int]) = {
    var min = arr.min
    val mod52sum = arr.filter(x => (x - min) % 5 == 3).map(x => findStepsRequired(x + 2 - min)).sum
    val sum = arr.filter(x => (x - min) % 5 != 3).map(_ + 2).map(x => findStepsRequired(x - min)).sum
    mod52sum + sum
  }

  def noMod5(arr: Array[Int]) = {
    var min = arr.min
    val sum = arr.map(x => findStepsRequired(x - min)).sum
    sum
  }

  for (i <- 2 to 200 if (i % 2 == 0)) {
    val arr = line(i).split(' ').map(_.toInt)
    //println(s"${mod5To1(arr)}, ${mod5To2(arr)}, ${noMod5(arr)}")
    println(Array(mod5To1(arr), mod5To2(arr), noMod5(arr)).min)
  }

}
