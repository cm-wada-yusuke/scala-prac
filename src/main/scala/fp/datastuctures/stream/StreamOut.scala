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


    println(InfinityStream.unfoldOnes.take(20).toList)

    println(InfinityStream.constant("c").take(20).toList)
    println(InfinityStream.unfoldConstant("d").take(20).toList)

    println(InfinityStream.from(10).take(20).toList)
    println(InfinityStream.unfoldFrom(10).take(20).toList)

    println(InfinityStream.fibs.take(20).toList)
    println(InfinityStream.unfoldFibs.take(20).toList)

  }
}
