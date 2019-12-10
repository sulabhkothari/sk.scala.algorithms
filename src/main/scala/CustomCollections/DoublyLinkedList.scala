package CustomCollections


case class Node[T](var prev: Node[T], var next: Node[T], var value: T) {
  override def toString: String = value.toString
}

case class DoublyLinkedList[A](var first: Node[A], var last: Node[A]) {

  private class Iter extends Iterator[Node[A]] {
    private var nextNode = first

    override def hasNext: Boolean = nextNode != null

    override def next(): Node[A] = {
      val retNode = nextNode
      nextNode = nextNode.next
      retNode
    }
  }

  // Add to front and remove from back
  def refreshNodes(num: Int, defaultValue: A): Unit = {
    var firstNode: Node[A] = first
    var lastNode: Node[A] = last
    var prevNodeFirst = first
    var prevNodeLast = last
    for(step <- 1 to num) {
      firstNode = Node[A](null, firstNode, defaultValue)
      firstNode.next.prev = firstNode
      lastNode = lastNode.prev
      lastNode.next = null
    }

    first = firstNode
    last = lastNode
  }

  def iterator: Iterator[Node[A]] = new Iter

  def update(index: Int, value: A) = {
    var prevNode = last
    var count = index
    while(prevNode != null && count > 0) {
      prevNode = prevNode.prev
      count -= 1
    }
    prevNode.value = value
  }

  def apply(index: Int): A = {
    var prevNode = last
    var count = index
    while(prevNode != null && count > 0) {
      prevNode = prevNode.prev
      count -= 1
    }
    prevNode.value
  }
}

object DoublyLinkedList {
  def apply[T](n: Int, defaultValue: T): DoublyLinkedList[T] = {
    val first = Node[T](null, null, defaultValue)
    var prevNode = first
    for (_ <- 1 to n-1) {
      prevNode.next = Node[T](null, null, defaultValue)
      prevNode.next.prev = prevNode
      prevNode = prevNode.next
    }
    val last = prevNode
    last.next = null
    DoublyLinkedList(first, last)
  }
}
