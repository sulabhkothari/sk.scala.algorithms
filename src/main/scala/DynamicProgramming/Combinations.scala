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

object CC extends App {
  val str = "abcdefgh".toCharArray
  val k = 4
  val d = mutable.Set.empty[String]

  println(str.length)
  val cc = str.slice(0, k)
  println(cc.mkString(""))
  var count = 1

  combinate(0,k)

  def combinate(arrPtr: Int, elPtr: Int): Unit = {
    var ptr = elPtr
    while(arrPtr < k && ptr < str.length) {
      combinate(arrPtr + 1, ptr)
      cc(arrPtr) = str(ptr)
      count += 1
      println(cc.mkString(""))
      d.add(cc.mkString(""))

      ptr = ptr + 1
    }
    if(arrPtr < k) cc(arrPtr) = str(arrPtr)
  }

  println(count)
}
