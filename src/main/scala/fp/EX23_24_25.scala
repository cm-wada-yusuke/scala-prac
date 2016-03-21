package fp

/**
  * Created by y-wada on 2016/03/21.
  */
object EX23_24_25 {

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
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { a => f(a, _) }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a, b) => f(a)(b) }

  def compose[A, B, C](f: B => C, g: A => B): A => C = { a => f(g(a)) }


  def main(args: Array[String]) {
    val add = (x: Int, y: Int) => x + y
    val curryiedAdd = curry(add)

    println("curried: curriedAdd(2)(4):")
    println(curryiedAdd(2)(4))

    val uncurriedAdd = uncurry(curryiedAdd)
    println("curried: uncurriedAdd(2, 4):")
    println(uncurriedAdd(2, 4))

    val add10 = curryiedAdd(10)
    val multi5 = curry((a: Int, b: Int) => a * b)(5)
    println("compose: compose(multi5, add10)(7): expected => 85")
    println(compose(multi5, add10)(7))
  }
}
