package fp

import fp.datastuctures.List._
import fp.datastuctures._

/**
  * Created by y-wada on 2016/03/21.
  */
object EX31_32_33 {

  /**
    * 3つ目にマッチするので1 + 2 で3が返るはず。
    */
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def main(args: Array[String]) {
    // EX31
    println(x)

    //EX32
    val l = List(1, 2, 3, 4, 5)
    println(tail(l))

    //EX33
    println(setHead(10, l))

    //EX34
    println(drop(l, 3))

    //EX35
    println(dropWhile(l, (x: Int) => x < 5))
  }
}
