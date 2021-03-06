package fp.output

import fp.datastuctures.list
import fp.datastuctures.list.Cons
import fp.datastuctures.list.List._

/**
  * Created by y-wada on 2016/03/21.
  */
object EX3_ {


  /**
    * EX31.
    * 3つ目にマッチするので1 + 2 で3が返るはず。
    */
  val x = list.List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case list.Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def main(args: Array[String]) {
    // EX31
    println("EX31")
    println(x)

    //EX32
    println()
    println("EX32")
    val l = list.List(1, 2, 3, 4, 5)
    println(tail(l))

    //EX33
    println()
    println("EX33")
    println(setHead(10, l))

    //EX34
    println()
    println("EX34")
    println(drop(l, 3))

    //EX35
    println()
    println("EX35")
    println(dropWhile(l, (x: Int) => x < 5))

    //EX36
    println()
    println("EX36")
    println(init(l))

    //EX37
    println()
    println("EX37")
    val l2 = list.List(1.0, 2.0, 3.0, 4.0, 5.0)
    println(s"$l2")
    println(product3(l2))

    val l3 = list.List(1.0, 2.0, 0.0, 4.0, 5.0)
    println("EX37")
    println(s"$l3")
    println(product3(l3))

    //EX38
    /**
      * NilとConsを与えるとコンストラクタで生成する結果（つまり入力したリストと同じ）結果になる。
      * Listのデータ型がNilとConsであると考えると、
      * Listコンストラクタは、foldRigtの初期値と構築するための関数をfoldRightに渡すことで
      * コンストラクタを実装できることがいえる。両者は本質的に同じことをやっている。
      */
    println()
    println("EX38")
    println(s"foldRight to List")
    println(foldRight(list.List(1, 2, 3), list.Nil: list.List[Int])(Cons(_, _)))

    //EX38
    println()
    println("EX38")
    println(length(l))

    //EX311
    println()
    println("EX311")
    println(sumL(l))
    println(productL(l2))
    println(lengthL(l))


    //EX312
    println()
    println("EX312")
    println(reverse(l))
    println(reverse2(l))


    //EX313
    println()
    println("EX313")
    println(foldRightByFoldLeft(list.List(1, 2, 3), 0)(_ + _))
    println(foldRight(list.List(1, 2, 3), 0)(_ + _))


    //EX314
    println()
    println("EX314")
    println(appendFold(l, l))

  }
}
