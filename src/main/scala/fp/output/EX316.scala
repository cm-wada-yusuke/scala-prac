package fp.output

import fp.datastuctures.List
import fp.datastuctures.List._

/**
  * Created by y-wada on 2016/03/24.
  */
object EX316 {

  def main(args: Array[String]) {

    val l = List(1, 2, 3, 4, 5)
    val l2 = List(1, 2, 3, 4, 5)
    val dl = List(1.0, 2.0, 3.0, 4.0, 5.0)

    val s = List("a", "b", "c")
    val s2 = List("d", "e", "f")

    //EX316
    println()
    println("EX316")
    println(addOneEach(l))

    //EX317
    println()
    println("EX317")
    println(doubleToString(dl))

    //EX318
    println()
    println("EX318")
    println(map(l)(_ + 1))

    //EX319
    println()
    println("EX319")
    println(filterOdd(l))
    println(str(filterOdd(map(l)(_ + 1))))

    //EX320
    println()
    println("EX320")
    println(str(flatMap(l)(i => List(i, i)))) //EX320

    //EX321
    println()
    println("EX321")
    println(str(flatMapFilter(l)((_ % 2 == 0))))

    //EX322
    println()
    println("EX322")
    println(str(zipWithAdd(l, l2))) //EX322

    //EX323
    println()
    println("EX323")
    println(str(zipWith(s, s2)((a1, a2) => List(a1 + a2))))
  }
}
