package fp.output

import fp.datastuctures.Tree._
import fp.datastuctures.{Branch, Leaf}

/**
  * Created by wada.yusuke on 2016/05/16.
  */
object Tree {

  def main(args: Array[String]) {

    val st = Branch(Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d")))
    val it = Branch(
      Branch(Leaf(255), Leaf(0)),
      Branch(Leaf(300), Leaf(-10)))

    println(string(it))

    //EX325
    println()
    println("EX325")
    println(size(st))

    //EX326
    println()
    println("EX326")
    println(maximum(it))

    //EX327
    println()
    println("EX327")
    println(depth(it, 72))

    //EX328
    println()
    println("EX328")
    println(map(it)(_.toString.length))

    //EX329
    println()
    println("EX329-foldSize")
    println(foldSize(st))
    println("EX329f-foldMaximum")
    println(foldMaximum(it))
  }
}
