package DynamicProgramming

import scala.collection.mutable

object Combinations extends App {
  val arr = Array('a', 'b', 'c', 'd', 'e', 'f', 'g')
  val n = arr.length
  val k = 6

  case class CharArray(c: Array[Char], next: Int)

  val inQueue = mutable.Queue[CharArray]()
  arr.zipWithIndex.foreach { case (c, i) => inQueue.enqueue(CharArray(Array(c), i)) }

  var kFlag = true

  while (kFlag) {
    //println(inQueue.map(_.c.mkString(":")).mkString("..."))
    val ca = inQueue.dequeue()
    if (ca.c.length < k) {
      for (j <- ca.next + 1 to arr.length - 1) {
        inQueue.enqueue(CharArray(ca.c :+ arr(j), j))
      }
    }
    else {
      inQueue.enqueue(ca)
      kFlag = false
    }
  }

  inQueue.foreach(x => println(x.c.mkString(".")))
  println(inQueue.size)

  def factorial(num: Int) = (2 to num).foldLeft(1){case(f,x) => f * x}
  println(factorial(n)/(factorial(k) * factorial(n - k)))
}
