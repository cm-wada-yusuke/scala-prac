package dwango

/**
  * 制御構文の練習問題
  */
object ControlStatement {

  def main(args: Array[String]): Unit = {

    var age = 7
    var isSchoolStarted = false

    if (1 <= age && age <= 6 && !isSchoolStarted) {
      println("幼児です")
    }
    else {
      println("幼児ではありません")
    }

    loopFrom0To9()
    //    pythagoras()
    finger5()
  }

  def loopFrom0To9(): Unit = {
    var i = 0
    do {
      println(i)
      i += 1
    } while (i < 10)
  }

  def pythagoras(): Unit =
    for {
      a <- 1 to 1000
      b <- 1 to 1000
      c <- 1 to 1000
      if Math.pow(a, 2) == Math.pow(b, 2) + Math.pow(c, 2)
    } {
      println("------")
      println(s"a=$a")
      println(s"b=$b")
      println(s"c=$c")
      println("------")
    }

  def finger5(): Unit = {
    for (i <- 1 to 1000) {
      val randomList = new scala.util.Random(new java.security.SecureRandom()).alphanumeric.take(5).toList
      val result = randomList match {
        case List(a, b, c, d, _) => List(a, b, c, d, a).mkString
      }
      println(s"$i : $result")
    }


  }


}
