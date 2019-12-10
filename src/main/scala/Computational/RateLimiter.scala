package Computational.test

import org.joda.time.DateTime
import CustomCollections.DoublyLinkedList

trait RateLimiter {
  def getAllBuckets(): List[Long]

  var startDateTime: DateTime

  def bucketSizeInSeconds: Long

  def windowSizeInSeconds: Long

  def allowedRatePerWindow: Long

  protected val windowCount = (windowSizeInSeconds / bucketSizeInSeconds).toInt

  protected def numOfWindowsSinceStart(currentDateTime: DateTime) = (currentDateTime.getMillis - startDateTime.getMillis) / (1000 * bucketSizeInSeconds)

  def slideWindow(currentDateTime: DateTime) = {
    println(s"Buckets: ${getAllBuckets().mkString(",")}")
    val slide = (numOfWindowsSinceStart(currentDateTime) - windowCount).toInt + 1
    println(s"startime: $startDateTime,currentime: $currentDateTime,slide: $slide, diff: ${(currentDateTime.getMillis - startDateTime.getMillis) / (1000 * bucketSizeInSeconds)}")
    if (slide > 0) {
      performSlide(slide)
      startDateTime = new DateTime(startDateTime.getMillis + slide * bucketSizeInSeconds * 1000)
    }
  }

  def sendRequest(): Boolean = {
    val currentDt = DateTime.now()
    slideWindow(currentDt)
    val index = numOfWindowsSinceStart(currentDt).toInt
    if (totalRequestsInWindow < allowedRatePerWindow) {
      this (index.toInt) += 1
      true
    }
    else
      false
  }

  def performSlide(steps: Int): Unit

  def totalRequestsInWindow(): Long

  def update(index: Int, value: Long)

  def apply(index: Int): Long
}

case class RateLimiterWithArray(override val bucketSizeInSeconds: Long,
                                override val windowSizeInSeconds: Long,
                                override val allowedRatePerWindow: Long) extends RateLimiter {

  val arr = Array.ofDim[Long](windowCount)
  var start: Int = 0
  override var startDateTime = DateTime.now()

  override def performSlide(steps: Int) = {
    for (step <- 0 to steps - 1) {
      arr((start + step) % windowCount) = 0
    }
    start = (start + steps) % windowCount
  }

  def printCircularArr = {
    for (i <- start to start + windowCount - 1) print(s"${arr(i % windowCount)},")
  }

  override def apply(index: Int): Long = arr((start + index.toInt) % windowCount)

  override def update(index: Int, value: Long): Unit = {
    arr((start + index.toInt) % windowCount) = value
  }

  override def totalRequestsInWindow(): Long = arr.sum

  override def getAllBuckets(): List[Long] = arr.toList
}

case class RateLimiterWithDoublyLinkedList(override val bucketSizeInSeconds: Long,
                                               override val windowSizeInSeconds: Long,
                                               override val allowedRatePerWindow: Long) extends RateLimiter {
  val arr = Array.ofDim[Long](windowCount)
  var start: Int = 0
  var startDateTime = DateTime.now()
  val dll = DoublyLinkedList[Long](windowCount, 0)

  override def performSlide(steps: Int): Unit = dll.refreshNodes(steps, 0)

  override def totalRequestsInWindow(): Long = dll.iterator.map(_.value).sum

  override def update(index: Int, value: Long): Unit = {
    dll(index) = value
  }

  override def apply(index: Int) = dll(index)

  override def getAllBuckets(): List[Long] = dll.iterator.map(_.value).toList.reverse
}

object RateLimiterRunner extends App {
  val l = RateLimiterWithDoublyLinkedList(1, 10, 100)
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

import org.scalatest._

class RateLimiterTests extends Matchers with WordSpecLike {
  "Rate Limiter" should {
    "limit rate with 10 buckets of one second each with RateLimiterWithArray" in {
      val l = RateLimiterWithArray(1, 10, 100)
      for (i <- 1 to 200) {
        i match {
          case 99 =>
            println("Sleeping for sometime...")
            l.totalRequestsInWindow() shouldBe 98
            l.getAllBuckets() shouldBe List(98, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            Thread.sleep(8000)
          case 150 =>
            println("Sleeping for sometime...")
            l.getAllBuckets() shouldBe List(98, 0, 0, 0, 0, 0, 0, 0, 2, 0)
            Thread.sleep(5000)
          case _ =>
            println(s"Request# $i")
            l.sendRequest
        }
      }
      l.getAllBuckets() shouldBe List(0, 0, 0, 50, 0, 0, 0, 0, 2, 0)
    }

    "limit rate with 8 buckets of 2 second each with RateLimiterWithArray" in {
      val l = RateLimiterWithArray(2, 8, 20)
      for (i <- 1 to 50) {
        i match {
          case 15 =>
            println("Sleeping for sometime...")
            l.totalRequestsInWindow() shouldBe 14
            l.getAllBuckets() shouldBe List(14, 0, 0, 0)
            Thread.sleep(6000)
          case 20 =>
            println("Sleeping for sometime...")
            l.getAllBuckets() shouldBe List(14, 0, 0, 4)
            Thread.sleep(5000)
          case _ =>
            println(s"Request# $i")
            l.sendRequest
        }
      }
      l.getAllBuckets() shouldBe List(0, 16, 0, 4)
    }

    "limit rate with 10 buckets of one second each with RateLimiterWithDoublyLinkedList" in {
      val l = RateLimiterWithDoublyLinkedList(1, 10, 100)
      for (i <- 1 to 200) {
        i match {
          case 99 =>
            println("Sleeping for sometime...")
            l.totalRequestsInWindow() shouldBe 98
            l.getAllBuckets() shouldBe List(98, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            Thread.sleep(8000)
          case 150 =>
            println("Sleeping for sometime...")
            l.getAllBuckets() shouldBe List(98, 0, 0, 0, 0, 0, 0, 0, 2, 0)
            Thread.sleep(5000)
          case _ =>
            println(s"Request# $i")
            l.sendRequest
        }
      }
      l.getAllBuckets() shouldBe List(0, 0, 0, 0, 2, 0, 0, 0, 0, 50)
    }

    "limit rate with 8 buckets of 2 second each with RateLimiterWithDoublyLinkedList" in {
      val l = RateLimiterWithDoublyLinkedList(2, 8, 20)
      for (i <- 1 to 50) {
        i match {
          case 15 =>
            println("Sleeping for sometime...")
            l.totalRequestsInWindow() shouldBe 14
            l.getAllBuckets() shouldBe List(14, 0, 0, 0)
            Thread.sleep(6000)
          case 20 =>
            println("Sleeping for sometime...")
            l.getAllBuckets() shouldBe List(14, 0, 0, 4)
            Thread.sleep(5000)
          case _ =>
            println(s"Request# $i")
            l.sendRequest
        }
      }
      l.getAllBuckets() shouldBe List(0, 4, 0, 16)
    }
  }
}
