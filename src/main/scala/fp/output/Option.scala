package fp.output

import fp.datastuctures.Option

/**
  * Optionの出力を確認するクラス
  */
object OptionOut {

  def main(args: Array[String]): Unit = {
    val dic = Map(12 -> "twelve", 24 -> "twenty four")

    //EX4.1
    println()
    println("EX4.1 map")
    println(Some(24).map(_ * 2))

    println()
    println("EX4.1 flatMap")
    println(Some(24).flatMap(key => Some(key * 3)))

    println()
    println("EX4.1 getOrElse")
    println(Some(24).getOrElse(12))
    println(None.getOrElse(12))

    println()
    println("EX4.1 orElse")
    println(Some(24).orElse(Some(12)))
    println(None.orElse(Some(12)))

    println()
    println("EX4.1 filter")
    println(Some(24).filter(_ % 2 == 0))
    println(Some(23).filter(_ % 2 == 0))

    val data = Seq(23.0, 44.0, 55.0, 14.0, 46.0, 13.0, 87.0)
    println()
    println("EX4.2 variance")
    println(Option.variance(data))

  }

}