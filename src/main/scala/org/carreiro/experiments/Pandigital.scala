package org.carreiro.experiments

object Pandigital extends App {

  override def main(args: Array[String]): Unit = {
    val low = (math.sqrt(1023456789L) - 0.5).toLong
    val high = (math.sqrt(9876543210L) + 0.5).toLong

    var count = 0
    for (l <- low to high) {
      val lsquared = l * l
      if (isNonReduntantPandigital(lsquared)) {
        count = count + 1
        println(s"$count) $lsquared is the square of $l")
      }
    }
  }

  private def isNonReduntantPandigital(n: Long): Boolean = {
    val numString = n.toString
    (numString.length == 10) && (numString.distinct.length == 10)
  }
}
