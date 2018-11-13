import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object main {
  //TTFTFFTFT
  //|&^^&&|^
  var flags: Array[Boolean] = Array(true, true, false, true, false, false, true, false, true)
  var ops: Array[FlagOps] = Array(Or, And, Xor, Xor, And, And, Or, Xor)


  var mapTrue = new Array[Int](9)
  var mapFalse = new Array[Int](9)

  def main(args: Array[String]): Unit = {
    val num =
      calculate(List(true, true, false, true, false, false, true, false, true),List(Or, And, Xor, Xor, And, And, Or, Xor),true)
    println(num)

    println(calculate(List(true, true, false, true),List(Or, And, Xor),true))

    println(calculate(List(true, false, true),List(Or, And),true))

    println(calculate(List(true, false, true),List(Xor, And),true))

    println(calculate(List(true, false, false),List(Xor, Or),true))
  }

  def getCount = {
    mapTrue(0) = if (flags(0)) 1 else 0
    mapFalse(0) = if (!flags(0)) 1 else 0

    mapTrue(1) = if (ops(0)(flags(0), flags(1))) 1 else 0
    mapFalse(1) = if (!ops(0)(flags(0), flags(1))) 1 else 0

    for (i <- 2 to flags.length - 1) {
      evaluate(i, i, flags(i), ops(i - 1))
      evaluate(i, i - 1, ops(i - 1)(flags(i), flags(i - 1)), ops(i - 2))
    }
  }

  def evaluate(indexToUpdate: Int, index: Int, currFlag: Boolean, currOp: FlagOps) = {
    currOp match {
      case Or =>
        if (currFlag) {
          mapTrue(indexToUpdate) += mapTrue(index - 1) + mapFalse(index - 1)
        }
        else {
          mapTrue(indexToUpdate) += mapTrue(index - 1)
          mapFalse(indexToUpdate) += mapFalse(index - 1)
        }

      case And =>
        if (!currFlag) {
          mapFalse(indexToUpdate) += mapTrue(index - 1) + mapFalse(index - 1)
        }
        else {
          mapTrue(indexToUpdate) += mapTrue(index - 1)
          mapFalse(indexToUpdate) += mapFalse(index - 1)
        }

      case Xor =>
        if (!currFlag) {
          mapFalse(indexToUpdate) += mapFalse(index - 1)
          mapTrue(indexToUpdate) += mapTrue(index - 1)
        }
        else {
          mapTrue(indexToUpdate) += mapFalse(index - 1)
          mapFalse(indexToUpdate) += mapTrue(index - 1)
        }
    }
  }

  def calculate(flags: List[Boolean], ops: List[FlagOps], value: Boolean): Int = {

    if(ops.isEmpty)
      if(flags.head == value) return 1
      else return 0

    var leftFlags = List(flags.head)
    var rightFlags = flags.tail
    var leftOps = List.empty[FlagOps]
    var rightOps = ops.tail

    var numberOfPos: Int = 0

    for (op <- ops) {
      val leftTrue = calculate(leftFlags, leftOps, true)
      val rightTrue = calculate(rightFlags, rightOps, true)
      val leftFalse = calculate(leftFlags, leftOps, false)
      val rightFalse = calculate(rightFlags, rightOps, false)

      numberOfPos += (op match {
        case Or =>
          if (value)
            leftTrue * rightFalse + leftFalse * rightTrue + leftTrue * rightTrue
          else
            leftFalse * rightFalse
        case And => 0
          if(value)
            rightTrue * leftTrue
          else
            leftTrue * rightFalse + leftFalse * rightTrue + leftFalse * rightFalse
        case Xor =>
          if(value)
            rightTrue*leftFalse + rightFalse*leftTrue
          else
            leftTrue*rightTrue + leftFalse*rightFalse

      })

      leftOps = leftOps :+ op
      rightOps = if(!rightOps.isEmpty) rightOps.tail else rightOps
      leftFlags = leftFlags :+ rightFlags.head
      rightFlags = rightFlags.tail
    }

    numberOfPos
  }
}

sealed trait FlagOps {
  def apply(op1: Boolean, op2: Boolean): Boolean
}

object Or extends FlagOps {
  def apply(op1: Boolean, op2: Boolean) = {
    op1 || op2
  }
}

object And extends FlagOps {
  def apply(op1: Boolean, op2: Boolean) = {
    op1 && op2
  }
}

object Xor extends FlagOps {
  def apply(op1: Boolean, op2: Boolean) = {
    (op1 && !op2) || (!op1 && op2)
  }
}
