package fp

import fp.datastuctures.List
import fp.datastuctures.List._

/**
  * Created by y-wada on 2016/03/24.
  */
object EX316 {

  def main(args: Array[String]) {

    val l = List(1, 2, 3, 4, 5)
    val dl = List(1.0, 2.0, 3.0, 4.0, 5.0)

    //EX314
    println()
    println("EX316")
    println(addOneEach(l))

    //EX315
    println()
    println("EX317")
    println(doubleToString(dl))

  }
}
