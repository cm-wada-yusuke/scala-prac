package fp.output

import fp.datastuctures.Some

/**
 * Optionの出力を確認するクラス
 */
object Option {

  def main(args: Array[String]): Unit = {
    val dic = Map(12 -> "twelve", 24 -> "twenty four")

    //EX4.1
    println()
    println("EX4.1 map")
    println(Some(24).map(_ * 2))

    println("EX4.1 flatMap")
    println(Some(24).flatMap(key => Some(key * 3)))


  }

}
