/**
  * Created by y-wada on 2016/03/06.
  */
object HelloWorld {
  def main(args: Array[String]): Unit = {
    var age: Int = 5
    var isSchoolStaterd: Boolean = false

    if (age >= 1 && age <= 6 && !isSchoolStaterd) println("幼児です")
    else println("幼児ではありあません")

    // while式の課題
    var i = 0
    do {
      println(i)
      i = i + 1
    } while (i <= 10)
  }
}
