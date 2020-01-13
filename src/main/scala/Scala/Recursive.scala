package Scala

import scala.annotation.tailrec

object Recursive extends App {
  def concatenateTailrec(str: String, n: BigInt): String = {

    @tailrec
    def concatenateHelper(str: String, n: BigInt, accumulator: String): String = {
      if (n <= 1)
        accumulator
      else
        concatenateHelper(str, n-1, accumulator + " " + str)

    }
    concatenateHelper(str, n, "")
  }

  def concatenate(str: String, n: Int): String = {

    println("on: " + n + ", str: " + str)
    if (n <= 1)
      str
    else
      str + " " + concatenate(str, n-1)
  }

  println(concatenateTailrec("hola", BigInt("90000000000")))

}
