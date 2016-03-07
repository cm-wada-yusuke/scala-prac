/**
  * Created by y-wada on 2016/03/07.
  */
class User(name: String, age: Int, private[this] val weight: Int)

object User {
  def main(args: Array[String]) {
    val jiro = new User("jiro", 22, 59)
//    println(jiro.weight)
  }
}
