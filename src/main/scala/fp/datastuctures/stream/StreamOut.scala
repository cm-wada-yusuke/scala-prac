package fp.datastuctures.stream

/**
 * 遅延リストの出力テスト。
 */
object StreamOut {

  def main(args: Array[String]) {
    println(Stream(1, 2, 3).take(2).toList)

    println(Stream(1, 2, 3).map(_ * 3).toList)

    println(Stream(1, 2, 3).filter(_ % 2 == 0).toList)

    println(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList)

    println(Stream(1, 2, 3).append(Stream("sd", "gg", "ab")).toList)


    println(InfiniteStream.unfoldOnes.take(20).toList)

    println(InfiniteStream.constant("c").take(20).toList)
    println(InfiniteStream.unfoldConstant("d").take(20).toList)

    println(InfiniteStream.from(10).take(20).toList)
    println(InfiniteStream.unfoldFrom(10).take(20).toList)

    println(InfiniteStream.fibs.take(20).toList)
    println(InfiniteStream.unfoldFibs.take(20).toList)

    print("unfoldMap: ")
    println(Stream(1, 2, 3).unfoldMap(_ + 10).toList)


    print("unfoldTake: ")
    println(Stream(1, 2, 3, 4, 5).unfoldTake(3).toList)


    print("unfoldTakeWhile: ")
    println(Stream(1, 2, 3, 4, 5).unfoldTakeWhile(_ < 4).toList)

    print("zipWith: ")
    println(Stream(1, 2, 3, 4, 5).zipWith(Stream(6, 7, 8))((a, b) => a + b).toList)


    print("zipAll: ")
    println(Stream(1, 2, 3, 4, 5).zipAll(Stream(6, 7, 8)).toList)

    print("startsWith: ")
    print(Stream(1, 2, 3, 4, 5).startsWith(Stream(6, 7, 8)))
    println(Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3)))

    print("tails: ")
    println(Stream(1, 2, 3, 4, 5).tails.map(_.toList).toList)


    print("scanRight: ")


  }
}
