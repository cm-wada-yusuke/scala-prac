package fp

/**
  * Created by y-wada on 2016/03/21.
  */
object EX23 {

  /**
    * ここでは関数の部分適用のためのアンダースコアを用いている。
    * 特定の引数のみ指定し、あとは別で指定したいという場合に、アンダースコアを使う。
    * 今回のように関数をカリー化したい場合や、多くの引数デフォルト値があり、
    * デフォルト値はそのままで特定の引数のみ指定させたい場合なども有効。
    *
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { (a: A) => f(a, _) }

  def main(args: Array[String]) {
    val add = (x: Int, y: Int) => x + y
    println(curry(add)(10)(23))
  }
}
