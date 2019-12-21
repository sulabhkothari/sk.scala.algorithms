package DynamicProgramming

import scala.collection.mutable.ArrayBuffer

object ArrayPartition extends App {
  //val random = new scala.util.Random()
  //val arr = Array.fill[Int](arrSize)(Random.nextInt(arrSize))
  val arr = Array(1, 5, 11, 5)
  arr.foreach(println)
  ArrayPartitioning.partition(arr).foreach(_.foreach(println))

}

object ArrayPartitioning {
  def partition(arr: Array[Int]): Option[Array[Int]] = {

    val sum = arr.sum
    if (sum % 2 != 0) return None

    val metaArr = Array.ofDim[Boolean](sum / 2 + 1, arr.length)
    for {
      row <- 0 to sum / 2
      col <- 0 until arr.length
    } {
      if (row == 0) metaArr(row)(col) = true
      else if (row - arr(col) > -1 && col > 0 && metaArr(row - arr(col))(col - 1)) metaArr(row)(col) = true
      else if (col > 0 && metaArr(row)(col - 1)) metaArr(row)(col) = true
    }

    for {
      row <- 0 to sum / 2
      col <- 0 until arr.length
    } {
      if (col == 0) println
      print(metaArr(row)(col) + ", ")
    }

    var row = sum / 2
    var col = arr.length - 1

    if (!metaArr(row)(col)) return None

    var trackSum = sum / 2
    val ab = new ArrayBuffer[Int]()
    while (trackSum > 0) {
      //println(s"$row,$col")
      if (!metaArr(row)(col - 1) || col == 0) {
        ab += arr(col)
        trackSum -= arr(col)
        row = trackSum
      }
      col -= 1
    }

    Some(ab.toArray)
  }
}
