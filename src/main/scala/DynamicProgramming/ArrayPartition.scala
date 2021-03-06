package DynamicProgramming

import java.util.concurrent.CountDownLatch

import scala.collection.mutable._

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

object MissingNumber extends App {
  val arr = Array(1,1,2)//Array(9, 8, 5, 1, 3, 2, 7, 6, -1)
  var i = 1
  while (i <= arr.length) {
    if (arr(i - 1) > 0 && arr(i - 1) != i && arr(i - 1) != arr(arr(i - 1) - 1)) {
      val temp = arr(i - 1) //9
      val temp2 = arr(temp - 1) // 1
      arr(i - 1) = temp2
      arr(temp - 1) = temp
      //println(s"$i,${arr(i-1)},${arr(arr(i-1)-1)}")
    }
    else {
      i += 1
    }
  }

  println(arr.mkString(","))
}

object CounddownLatchApp extends App {
  println("Starting...")
  val countDownLatch = new CountDownLatch(2)
  val hello = (num: Int) => new Thread(new Runnable {
    override def run() {
      Thread.sleep(2000)
      println(s"hello world: $num")
      countDownLatch.countDown()
    }
  })
  hello(1).start
  hello(2).start

  //runThread.start()
  //runThread(2).start()
  //runThread(3).start()

  //Thread.sleep(10000)

  countDownLatch.await()

  println("Complete!")

}
