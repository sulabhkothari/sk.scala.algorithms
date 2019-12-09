package Computational

import org.joda.time.DateTime

import scala.collection.mutable

case class RateLimiterWithArray(bucketSizeInSeconds: Long, windowSizeInSeconds: Long, allowedRatePerWindow: Long) {
  val windowCount = (windowSizeInSeconds / bucketSizeInSeconds).toInt
  val arr = Array.ofDim[Long](windowCount)
  var start: Int = 0
  var startDateTime = DateTime.now()

  def numOfWindowsSinceStart(currentDateTime: DateTime) = (currentDateTime.getMillis - startDateTime.getMillis) / (1000 * bucketSizeInSeconds)

  def slideWindow(currentDateTime: DateTime) = {
    val slide = (numOfWindowsSinceStart(currentDateTime) - windowCount).toInt + 1
    println(s"startime: $startDateTime,startindex: $start,currentime: $currentDateTime,slide: $slide, diff: ${(currentDateTime.getMillis - startDateTime.getMillis) / (1000 * bucketSizeInSeconds)}")
    if (slide > 0) {
      println("slide encountered")
      for (step <- 0 to slide - 1) {
        arr((start + step) % windowCount) = 0
      }
      start = (start + slide) % windowCount
      println(s"New Start of Arr: $start")
      //println(s"${startDateTime.getMillis}, ")
      startDateTime = new DateTime(startDateTime.getMillis + slide * bucketSizeInSeconds * 1000)
    }
  }

  def printCircularArr = {
    for (i <- start to start + windowCount - 1) print(s"${arr(i % windowCount)},")
  }

  def sendRequest(): Boolean = {
    printCircularArr
    val currentDt = DateTime.now()
    slideWindow(currentDt)
    val index = numOfWindowsSinceStart(currentDt).toInt
    println(s"Index: $index")
    println(s"Sum: ${arr.sum}")
    if (arr.sum < allowedRatePerWindow) {
      arr((start + index.toInt) % windowCount) += 1
      true
    }
    else
      false
  }
}

case class RateLimiterWithList(bucketSizeInSeconds: Int, windowSizeInSeconds: Int) {
  val scopeOfBuckets = mutable.ListBuffer.empty[Int]
}

object RateLimiterRunner extends App {
  val l = RateLimiterWithArray(1, 10, 100)
  var sum = 0
  for (i <- 1 to 200) {
    i match {
      case 99 =>
        println("Sleeping for sometime...")
        Thread.sleep(8000)
      case 150 =>
        println("Sleeping for sometime...")
        Thread.sleep(5000)
      case _ =>
        println(s"Request# $i")
        if (l.sendRequest) sum += 1
    }
  }
  println(sum)
}
