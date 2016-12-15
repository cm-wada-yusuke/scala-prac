package dwango

import scala.io.Source

object Func {

  def withFile[A](filename: String)(f: Source => A): A = {
    val s = Source.fromFile(filename)
    try {
      f(s)
    } finally {
      s.close()
    }
  }

  def printFile(filename: String): Unit = {

    def printLine(s: Source): Unit =
      s.getLines().foreach(println)

    withFile(filename)(printLine)

  }

  def main(args: Array[String]): Unit =
    printFile(".gitignore")

}
