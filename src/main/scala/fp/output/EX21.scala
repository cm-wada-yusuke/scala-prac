package fp.output

/**
  * Created by y-wada on 2016/03/20.
  */
object EX21 {

  /**
    * 美しい…。
    * 加速度的に値が増加することを考えると、本当はIntを使うべきではない。
    *
    * @param n
    * @return
    */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(t: Int, cur: Int, next: Int): Int =
      if (t <= 1) cur
      else if (t == 0) throw new RuntimeException("cant take zero value.")
      else go(t - 1, next, cur + next)
    go(n, 0, 1)
  }

  /**
    * リストで並べてみたかった。最初、List.tabulate()で実装しようとしたがどうしてもzeroが排除できず断念。
    *
    * @param n
    * @return
    */
  def fibs(n: Int): List[Int] = {
    List.range(1, n).map(fib)
  }

  def main(args: Array[String]) {
    println(fib(5))
    println(fibs(10))
  }

}
