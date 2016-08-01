//package four

object For {

  def main(args: Array[String]): Unit = {
    println(yields)
  }

  def simpleFour = {
    val list = List(1, 2, 3, 4, 5)
    for {
      l <- list
    } {
      println(l)
    }
  }

  def yields = {
    val list = List(1, 2, 3, 4, 5)
    for {
      l <- list
    } yield (l)
  }


}
