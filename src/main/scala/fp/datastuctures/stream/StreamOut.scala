package fp.datastuctures.stream

/**
  * 遅延リストの出力テスト。
  */
object StreamOut {

  def main(args: Array[String]) {
    println(Stream(1, 2, 3).take(2).toList)
  }

}
