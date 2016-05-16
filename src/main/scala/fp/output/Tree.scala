package fp.output

import fp.datastuctures.Tree._
import fp.datastuctures.{Branch, Leaf}

/**
  * Created by wada.yusuke on 2016/05/16.
  */
object Tree {

  def main(args: Array[String]) {

    val t = Branch(Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d")))

    //EX325
    println()
    println("EX316")
    println(size(t))
  }
}
