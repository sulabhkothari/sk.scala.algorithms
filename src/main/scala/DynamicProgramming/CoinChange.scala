package DynamicProgramming

import scala.collection.mutable


object CoinChange {
  def main(args: Array[String]): Unit = {
    println(calculate(4, Array(1, 2, 3)))
    println(calculate(4000, Array(2, 5, 3, 6)))
  }

  def incorrectCalculate(n: Int, coins: Array[Int]) = {
    val mapOfCoinDistribution = mutable.Map.empty[Int, Int]
    coins.foreach(i => mapOfCoinDistribution += i -> 1)
    println(mapOfCoinDistribution)
    for {
      x <- 1 to n
      y <- 1 to x / 2
    } {
      if (!mapOfCoinDistribution.contains(x)) mapOfCoinDistribution += x -> 0
      if (mapOfCoinDistribution.contains(y) && mapOfCoinDistribution.contains(x - y))
        if (x - y == y)
          mapOfCoinDistribution(x) += mapOfCoinDistribution(y) * (mapOfCoinDistribution(y) + 1) / 2
        else
          mapOfCoinDistribution(x) += mapOfCoinDistribution(y) * mapOfCoinDistribution(x - y)
    }
    println(mapOfCoinDistribution)
    mapOfCoinDistribution(n)
  }

  def calculate(n: Int, coins: Array[Int]) = {
    var memTable = Array.ofDim[Int](n + 1, coins.length + 1)
    for (j <- 0 to coins.length)
      memTable(0)(j) = 1
    for {
      i <- 1 to n
      j <- 1 to coins.length
    } {
      memTable(i)(j) = memTable(i)(j - 1) + (if (i - coins(j - 1) >= 0) memTable(i - coins(j - 1))(j) else 0)
    }

    memTable(n)(coins.length)
  }

  def print2DArray(arr: Array[Array[Int]]) = {
    for {
      i <- 0 to arr.length - 1
      j <- 0 to arr(i).length - 1
    } {
      print(s"${arr(i)(j)} ${if (j == arr(i).length - 1) "\n" else ""}")
    }
  }
}
