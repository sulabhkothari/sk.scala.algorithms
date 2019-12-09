package Computational

import scala.util.control.Breaks._

object PythagorasTriplets extends App {
  var p = 0
  var xPlusY = 0
  var twice_xy = 0

  breakable {
    for (z <- 1 to 500) {
      xPlusY = 1000 - z
      twice_xy = (xPlusY * xPlusY - z * z)
      val v = 4 * xPlusY * xPlusY - 8 * twice_xy
      if (v > 0) {
        val srv = Math.pow(v, 0.5)

        val x = ((2 * xPlusY + srv) / 4).toInt
        val y = ((2 * xPlusY - srv) / 4).toInt
        if (x * x + y * y == z * z) {
          println(s"$x,$y,$z")
          break
        }
      }
    }
  }
}

// x*x + y*y = z*z
// (x + y) ^ 2 = (1000-z)^2 = x*x + y*y +2xy = z*z + 2xy = 10^6 + z*z - 2000*z
// 2xy = 10^6 - 2000 * z
// x + y = 1000 - z
// x + y = a, 2xy = b => y = b/2x
//  x + b/2x = a => 2x^2 - 2ax + b = 0 => 2a +- (4a^2- 8b)/4
// a + b +  c = n
// a*a + b*b = (n - a -  b)^2 = n*n + a*a + b*b + 2ab - 2na-2nb => 2b(n - a) = n^2 - 2na => b = (n^2 - 2na)/(2(n-a))


object PythagorasTripletsSimpler extends App {
  val n = 1000
  breakable {
    for (a <- 1 to n / 3) {
      val b: Double = (n * n - 2 * a * n) / (2 * (n - a)).toDouble
      val c: Double = 1000 - a - b
      if (Math.floor(b) == b && a * a + b * b == c * c) {
        println(s"$a,${b.toInt},${c.toInt}")
      }
    }
  }
}
