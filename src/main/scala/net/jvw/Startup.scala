package net.jvw

object Startup {

  val FROM_HEART_RATE = 125
  val MAX_SPEED = 30

  def main(args: Array[String]) {
    val output = Convert.processFile(args(0))
    val result = output.filter(_._1 > FROM_HEART_RATE).filter(_._2 < MAX_SPEED)
    result.foreach((x) => println("" + x._1 + ", " + x._2))
  }


}
